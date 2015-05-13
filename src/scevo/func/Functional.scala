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

// Does this make sense? To reduce the number of parameters?
trait Environment extends Options with Collector
class EnvFromArgs(args: Array[String]) extends OptionsFromArgs(args) with CollectorFile with Environment

object IterativeAlgorithm {
  /*
  def apply[S <: State](init: Unit => S)(step: S => S)(stop: Seq[S => Boolean])(epilogue: S => S) : Unit => S = 
  def apply[S <: State](init: Unit => S)(step: S => S)(stop: S => Boolean*)(epilogue: S => S) : Unit => S = {
    apply(init)(step)(stop)(epilogue)
    */
  /*
  def apply[S <: State](init: Unit => S)(step: S => S)(stop: Seq[S => Boolean])(epilogue: S => S): Unit => S = {
    @tailrec def iterate(s: S): S = stop.forall((sc: S => Boolean) => !sc(s)) match {
      case false => s
      case true  => iterate(step(s))
    }
    init andThen iterate andThen epilogue
  }
  * 
  */
  def apply[S <: State](step: S => S)(stop: Seq[S => Boolean]): S => S = {
    @tailrec def iterate(s: S): S = stop.forall((sc: S => Boolean) => !sc(s)) match {
      case false => s
      case true  => iterate(step(s))
    }
    iterate
  }
  def apply[S <: Solution, E <: Evaluation](env: Environment)(step: StatePop[Tuple2[S, E]] => StatePop[Tuple2[S, E]])(stop: Seq[StatePop[Tuple2[S, E]] => Boolean]): StatePop[Tuple2[S, E]] => StatePop[Tuple2[S, E]] = {
    val bsf = new BestSoFar[S, E]
    def reporting = bsf(env, env)
    apply(step andThen reporting)(stop) andThen EpilogueBestOfRun[S, E](bsf, env)
  }
}

// Evaluates population solution by solution (other modes of evaluation possible, e.g., in IFS)
object IndependentEval{
  def apply[S <: Solution, E <: Evaluation](f: S => E) =
    (s: StatePop[S]) => StatePop(s.solutions.map(x => (x, f(x))), s.iteration)
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

object Termination {
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
  def apply[S <: Solution, E <: Evaluation](config: Options) = Seq(
    MaxIter[StatePop[Tuple2[S, E]]](config),
    MaxTime(config))
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
      coll.rdb.setResult("bestOfRun.fitness", if (bsf.bestSoFar.isDefined) bsf.bestSoFar.get._2 else "NaN")
      coll.rdb.setResult("bestOfRun.genotype", bsf.bestSoFar.toString)
      coll.rdb.write("bestOfRun", bsf.bestSoFar)
      state
    }
}

object Experiment {
  def launch[S <: State](alg: Unit => S, env: Environment) = {
    _: Unit =>
      {
        val startTime = System.currentTimeMillis()
        try {
          env.warnNonRetrieved
          val state = alg()
          env.rdb.put("status", "completed")
          if (env.paramString("saveLastState", "false") == "true")
            env.rdb.write("lastState", state)
          Some(state)
        } catch {
          case e: Exception => {
            env.rdb.put("status", "error: " + e.getLocalizedMessage + e.getStackTrace().mkString(" ")) // .toString.replace('\n', ' '))
            throw e
          }
        } finally {
          env.close
          env.rdb.setResult("totalTimeSystem", System.currentTimeMillis() - startTime)
          env.rdb.setResult("system.endTime", Calendar.getInstance().getTime().toString)
          None
        }
      }
  }
}

// Use case: MaxOnes with GA

object TestGA {
  class S(val v: Vector[Boolean]) extends Solution {
    override val toString = v.map(if (_) "1" else "0").reduce(_ + _)
  }
  type E = ScalarEvaluationMax
  type ES = Tuple2[S, E] // Evaluated solution
  def evaluate(p: S) = ScalarEvaluationMax(p.v.count(b => b))

  def main(args: Array[String]): Unit = {
    val env = new EnvFromArgs(args)
    val rng = Rng(env)
    val numVars = env.paramInt("numVars", _ > 0)
    def initializer = RandomStatePop[S](env, () => new S(Vector.fill(numVars)(rng.nextBoolean)))

    // Prepare the functional components 
    def sel = TournamentSelection[S, E](env)(rng)
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

    def eval = IndependentEval(evaluate)
    def rmp = RandomMultiBreeder(rng, env)(operators(rng))
    def iteration = Breeder(rmp) andThen eval
    def stopBestFit = (s: StatePop[ES]) => s.solutions.exists(_._2.v == numVars)

    // Compose the components into search algorithm
    def alg = initializer andThen eval andThen
      IterativeAlgorithm[S, E](env)(iteration)(Termination[S, E](env) :+ stopBestFit)

    // Run the algorithm
    Experiment.launch[StatePop[Tuple2[S, E]]](alg, env)()
  }
}

object T {
  def main(args: Array[String]) {
    TestGA.main(Array("--numVars", "50", "--maxGenerations", "100"))
  }
}

/*
    val config = new OptionsFromArgs(args) // TODO: Detach options from collector
    //    def alg = IterativeAlgorithm[S,E](env)(initializer andThen IndependentEvaluation(evaluate))(iteration)(Termination[S, E](config) :+ stopMaxFit)
    //    def alg = IterativeAlgorithm[StatePop[ES]](initializer andThen eval)(iteration)(Termination[S, E](config) :+ stopMaxFit)(EpilogueBestOfRun[S, E](bsf, config))
trait Solver[Sol <: Solution] extends (() => Sol)

*/
