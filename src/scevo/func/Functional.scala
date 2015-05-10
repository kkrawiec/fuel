package scevo.func

import scala.annotation.tailrec
import scevo.Preamble.RndApply
import scevo.tools.Randomness
import scevo.evo.State
import scevo.evo.Solution
import scevo.evo.ScalarEvaluationMax
import scevo.tools.OptionsFromArgs
import scevo.evo.SeparableEvaluator
import scevo.tools.Options
import scevo.evo.Evaluation
import scevo.evo.BestSelector
import scevo.tools.Collector
import scevo.tools.Rng
import scevo.tools.TRandom
import scevo.tools.ResultDatabase


// Scevo-like functionality in functional programming style
// May 9th, 2015
// Other differences w.r.t. original scevo:
// - evaluated solution is now simply Tuple2[S,E] 
// TODO: still undecided if I need PopulationState or just Seq[Tuple2[S,E]]

trait PopulationState[S <: Solution, E <: Evaluation] extends State {
  def solutions: Seq[Tuple2[S, E]]
}

object PopulationState {
  def apply[S <: Solution, E <: Evaluation](sols: Seq[Tuple2[S, E]], iter: Int = 0): PopulationState[S, E] =
    new PopulationState[S, E] {
      require(sols.size > 0, "The set of working solutions in a state cannot be empty")
      override val solutions = sols
      override val iteration = iter
    }
}

// Function factories (component factories)

object IterativeAlgorithm {
  def apply[S <: State](init: Unit => S)(step: S => S)(stop: S => Boolean*)(epilogue: S => S) = {
    @tailrec def iterate(s: S): S = stop.forall((sc: S => Boolean) => !sc(s)) match {
      case false => s
      case true  => iterate(step(s))
    }
    init andThen iterate andThen epilogue
  }
}

// Evaluates population solution by solution (other modes of evaluation possible, e.g., in IFS)
object IndependentEvaluation {
  def apply[S <: Solution, E <: Evaluation](f: S => Tuple2[S, E]) =
    (s: Seq[S]) => PopulationState(s map f)
}

object PopulationBuilder {
  def apply[S <: Solution, E <: Evaluation](solutionBuilder: Seq[Tuple2[S, E]] => List[S]) = {
    current: Seq[Tuple2[S, E]] =>
      @tailrec def producePopulation(next: List[S]): Seq[S] =
        if (next.size >= current.size)
          next.take(current.size)
        else
          producePopulation(solutionBuilder(current) ++ next)
      producePopulation(List[S]())
  }
}

// Picks one of the functions (pipes) at random to 
object RandomMultiProducer {
  def apply[S <: Solution, E <: Evaluation](rng: TRandom)(pipes: Seq[Seq[Tuple2[S, E]] => List[S]]) =
    pipes(rng)
}

object TournamentSelection {
  def apply[S <: Solution, E <: Evaluation](opt: Options)(rand: TRandom) = {
    val tournamentSize = opt.paramInt("tournamentSize", 7, _ >= 2)
    pop: Seq[Tuple2[S, E]] => BestSelector(pop(rand, tournamentSize))
  }
}

object RandomPopulationState {
  def apply[S <: Solution, E <: Evaluation](opt: Options, solutionGenerator: () => Tuple2[S, E]) = {
    val populationSize = opt.paramInt("populationSize", 1000, _ > 0)
    _: Unit => PopulationState(for (i <- 0 until populationSize) yield solutionGenerator())
  }
}

object MaxGenerations {
  def apply[S <: State](opt: Options) = {
    val maxGenerations = opt.paramInt("maxGenerations", 50, _ > 0)
    s: S => s.iteration >= maxGenerations
  }
}

// Reporting
class BestSoFar[S <: Solution, E <: Evaluation] {
  protected var best: Option[Tuple2[S, E]] = None
  def bestSoFar: Option[Tuple2[S, E]] = best

  def apply(opt: Options, coll: Collector) = {
    val snapFreq = opt.paramInt("snapshot-frequency", 0)
    (s: PopulationState[S, E]) => {
      val bestOfGen = BestSelector(s.solutions)
      if (bestSoFar.isEmpty || bestOfGen._2.betterThan(best.get._2)) best = Some(bestOfGen)
      println(f"Gen: ${s.iteration}  BestSoFar: ${bestSoFar.get}")
      if (snapFreq > 0 && s.iteration % snapFreq == 0)
        coll.rdb.saveSnapshot(f"${s.iteration}%04d")
      s
    }
  }
}


// Use case: MaxOnes with GA

object Test {

  // Solution and evaluation
  class S(val v: Vector[Boolean]) extends Solution {
    override val toString = v.map(if (_) "1" else "0").reduce(_ + _)
  }
  type E = ScalarEvaluationMax
  def evaluate(p: S): Tuple2[S, E] = (p, ScalarEvaluationMax(p.v.count(b => b)))

  def main(args: Array[String]): Unit = {

    // Ugly hack: this object covers all technicalities about configuration and reporting, TODO
    val config = new OptionsFromArgs(args) with Rng with Collector
    val numVars = config.paramInt("numVars", _ > 0)
    def initializer = RandomPopulationState[S, E](config, () => evaluate(new S(Vector.fill(numVars)(config.rng.nextBoolean))))

    // Prepare the functional components 
    def sel = TournamentSelection[S, E](config)(config.rng)

    // Calling selection function needs to be delegated to search operators, because only 
    // they know how many calls they need (mutation - one, crossover - two).
    // Thus, the function list created by operators() comprises both selection and search moves
    def operators(rng: TRandom) = List(
      (source: Seq[Tuple2[S, E]]) => {
        val s = sel(source)._1
        val bitToMutate = rng.nextInt(s.v.size)
        List(new S(s.v.updated(bitToMutate, !s.v(bitToMutate))))
      },
      (source: Seq[Tuple2[S, E]]) => {
        val me = sel(source)._1
        val cuttingPoint = rng.nextInt(me.v.size)
        val (myHead, myTail) = me.v.splitAt(cuttingPoint)
        val (hisHead, hisTail) = sel(source)._1.v.splitAt(cuttingPoint)
        List(new S(myHead ++ hisTail), new S(hisHead ++ myTail))
      })

    def rmp = RandomMultiProducer(config.rng)(operators(config.rng))
    def reporting = (new BestSoFar[S, E])(config, config)

    def iteration = ((p: PopulationState[S, E]) => p.solutions) andThen PopulationBuilder(rmp) andThen IndependentEvaluation(evaluate) andThen reporting
    def stopMaxGen = (MaxGenerations[PopulationState[S, E]](config))
    def stopMaxFit = (s: PopulationState[S, E]) => s.solutions.exists(_._2.v == numVars)

    // Compose the components into search algorithm
    def alg = IterativeAlgorithm[PopulationState[S, E]](initializer)(iteration)(stopMaxGen, stopMaxFit)(reporting)

    // Run the algorithm
    alg()
  }
}

object T {
  def main(args: Array[String]) {
    Test.main(Array("--numVars", "50", "--maxGenerations", "100"))
  }
}

/*
object SolProducer {
  def apply[S <: Solution, E <: Evaluation](rand: TRandom)(sel: Seq[Tuple2[S, E]] => Tuple2[S, E])(search: Seq[(Seq[Tuple2[S, E]] => Tuple2[S, E]) => S]) {
    current: Seq[Tuple2[S, E]] =>
      {

      }

  }
}

trait Solver[Sol <: Solution] extends (() => Sol)

    def operators(rng: TRandom) = List(
      (source: Unit => Tuple2[B, E]) => {
        val s = source()._1
        val bitToMutate = rng.nextInt(s.v.size)
        List(new B(s.v.updated(bitToMutate, !s.v(bitToMutate))))
      },
      (source: Unit => Tuple2[B, E]) => {
        val me = source()._1
        val cuttingPoint = rng.nextInt(me.v.size)
        val (myHead, myTail) = me.v.splitAt(cuttingPoint)
        val (hisHead, hisTail) = source()._1.v.splitAt(cuttingPoint)
        List(new B(myHead ++ hisTail), new B(hisHead ++ myTail))
      })

*/
