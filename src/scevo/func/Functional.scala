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
import java.util.Calendar
import scevo.tools.Collector
import scevo.tools.Random
import scevo.tools.Collector
import scevo.tools.CollectorFile
import scevo.Distribution

/* Scevo-like functionality in functional programming style

 May 9th, 2015
 Other differences w.r.t. original scevo:
 - evaluated solution is now simply Tuple2[S,E] 
 - StatePop or just Seq[Tuple2[S,E]]
 
 TODO: 
  - NSGA
  - rename Options to Config?
  - flatten Collector
 */

trait StatePop[T] extends State {
  def solutions: Seq[T]
}

object StatePop {
  def apply[T](sols: Seq[T], iter: Int = 0) = new StatePop[T] {
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
    (s: StatePop[S]) => StatePop(s.solutions map f, s.iteration)
}

object Breeder {
  def apply[S <: Solution, E <: Evaluation](solutionBuilder: () => (Seq[Tuple2[S, E]] => List[S])) = {
    current: StatePop[Tuple2[S, E]] =>
      @tailrec def breed(next: List[S]): Seq[S] =
        if (next.size >= current.solutions.size)
          next.take(current.solutions.size)
        else
          breed(solutionBuilder()(current.solutions) ++ next)
      StatePop[S](breed(List[S]()), current.iteration + 1)
  }
}

// Picks one of the functions (pipes) at random to 
object RandomMultiBreeder {
  def apply[S <: Solution, E <: Evaluation](rng: TRandom, config: Options)(pipes: Seq[Seq[Tuple2[S, E]] => List[S]]) = {
    val prob = config.paramString("operatorProbs")
    val distribution = Distribution(
      if (prob.isDefined)
        prob.get.split(",").map(_.toDouble)
      else {
        println("Probability distribution for operators undefined. Equal probabilities set.")
        val p = List.fill(pipes.size - 1)(1.0 / pipes.size)
        (1.0 - p.sum) :: p
      })
    require(distribution.d.size == pipes.size, "Invalid number of operator probabilities")
    () => pipes(distribution(rng))
  }
}

object TournamentSelection {
  def apply[S <: Solution, E <: Evaluation](opt: Options)(rand: TRandom) = {
    val tournamentSize = opt.paramInt("tournamentSize", 7, _ >= 2)
    pop: Seq[Tuple2[S, E]] => BestSelector(pop(rand, tournamentSize))
  }
}

object RandomStatePop {
  def apply[S <: Solution](opt: Options, solutionGenerator: () => S) = {
    val populationSize = opt.paramInt("populationSize", 1000, _ > 0)
    _: Unit => StatePop[S](for (i <- 0 until populationSize) yield solutionGenerator())
  }
}

object Condition {
  object MaxIter {
    def apply[S <: State](opt: Options) = {
      val maxGenerations = opt.paramInt("maxGenerations", 50, _ > 0)
      s: S => s.iteration >= maxGenerations
    }
  }
  object MaxTime {
    def apply(opt: Options) = {
      val maxMillisec = opt.paramInt("maxTime", 86400000, _ > 0)
      val startTime = System.currentTimeMillis()
      def timeElapsed = System.currentTimeMillis() - startTime
      s: Any => timeElapsed > maxMillisec
    }
  }
}

// Reporting
class BestSoFar[S <: Solution, E <: Evaluation] {
  protected var best: Option[Tuple2[S, E]] = None
  def bestSoFar: Option[Tuple2[S, E]] = best

  def apply(opt: Options, coll: Collector) = {
    val snapFreq = opt.paramInt("snapshot-frequency", 0)
    (s: StatePop[Tuple2[S, E]]) => {
      val bestOfGen = BestSelector(s.solutions)
      if (bestSoFar.isEmpty || bestOfGen._2.betterThan(best.get._2)) best = Some(bestOfGen)
      println(f"Gen: ${s.iteration}  BestSoFar: ${bestSoFar.get}")
      if (snapFreq > 0 && s.iteration % snapFreq == 0)
        coll.rdb.saveSnapshot(f"${s.iteration}%04d")
      s
    }
  }
}

object EpilogueBestOfRun {
  def apply[S <: Solution, E <: Evaluation](bsf: BestSoFar[S, E], coll: Collector) =
    (state: StatePop[Tuple2[S, E]]) => {
      coll.rdb.setResult("lastGeneration", state.iteration)
      coll.rdb.setResult("bestOfRun.fitness", if( bsf.bestSoFar.isDefined) bsf.bestSoFar.get._2 else "NaN")
      coll.rdb.setResult("bestOfRun.genotype", bsf.bestSoFar.toString)
      coll.rdb.write("bestOfRun", bsf.bestSoFar)
      state
    }
}

object Experiment {
  def launch[S <: State](alg: Unit => S, opt: Options, coll: Collector) = {
    _: Unit =>
      {
        val startTime = System.currentTimeMillis()
        try {
          opt.warnNonRetrieved
          val state = alg()
          coll.rdb.put("status", "completed")
          if (opt.paramString("saveLastState", "false") == "true")
            coll.rdb.write("lastState", state)
          Some(state)
        } catch {
          case e: Exception => {
            coll.rdb.put("status", "error: " + e.getLocalizedMessage + e.getStackTrace().mkString(" ")) // .toString.replace('\n', ' '))
            throw e
          }
        } finally {
          coll.close
          coll.rdb.setResult("totalTimeSystem", System.currentTimeMillis() - startTime)
          coll.rdb.setResult("system.endTime", Calendar.getInstance().getTime().toString)
          None
        }
      }
  }
}

// Use case: MaxOnes with GA

object Test {

  // Solution and evaluation
  class S(val v: Vector[Boolean]) extends Solution {
    override val toString = v.map(if (_) "1" else "0").reduce(_ + _)
  }
  object S
  type E = ScalarEvaluationMax
  type ES = Tuple2[S, E]
  def evaluate(p: S) = (p, ScalarEvaluationMax(p.v.count(b => b)))

  def main(args: Array[String]): Unit = {
    val config = new OptionsFromArgs(args)  // TODO: Detach options from collector
    val coll = config // TODO
    val rng = Rng(config)
    val numVars = config.paramInt("numVars", _ > 0)
    def initializer = RandomStatePop[S](config, () => new S(Vector.fill(numVars)(rng.nextBoolean)))

    // Prepare the functional components 
    def sel = TournamentSelection[S, E](config)(rng)
    // Calling selection function needs to be delegated to search operators, because only 
    // they know how many calls they need (mutation - one, crossover - two).
    // Thus, the function list created by operators() comprises both selection and search moves
    def operators(rng: TRandom) = List(
      (source: Seq[ES]) => {
        val s = sel(source)._1
        val bitToMutate = rng.nextInt(s.v.size)
        List(new S(s.v.updated(bitToMutate, !s.v(bitToMutate))))
      },
      (source: Seq[ES]) => {
        val me = sel(source)._1
        val cuttingPoint = rng.nextInt(me.v.size)
        val (myHead, myTail) = me.v.splitAt(cuttingPoint)
        val (hisHead, hisTail) = sel(source)._1.v.splitAt(cuttingPoint)
        List(new S(myHead ++ hisTail), new S(hisHead ++ myTail))
      })

    def rmp = RandomMultiBreeder(rng, config)(operators(rng))
    val bsf = new BestSoFar[S, E]
    def reporting = bsf(config, coll)
    def eval = IndependentEvaluation(evaluate)

    def iteration = Breeder(rmp) andThen eval andThen reporting
    def stopMaxGen = (Condition.MaxIter[StatePop[ES]](config))
    def stopMaxTime = (Condition.MaxTime(config))
    def stopMaxFit = (s: StatePop[ES]) => s.solutions.exists(_._2.v == numVars)

    // Compose the components into search algorithm
    def alg = IterativeAlgorithm[StatePop[ES]](initializer andThen eval)(iteration)(stopMaxGen, stopMaxFit)(EpilogueBestOfRun[S, E](bsf, config))

    // Run the algorithm
    Experiment.launch[StatePop[Tuple2[S, E]]](alg, config, coll)()
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
