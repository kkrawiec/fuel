package scevo.func

import java.util.Calendar
import scala.annotation.tailrec
import scevo.Distribution
import scevo.Preamble.RndApply
import scevo.evo.BestSelector
import scevo.evo.Evaluation
import scevo.evo.Solution
import scevo.evo.State
import scevo.tools.Collector
import scevo.tools.CollectorFile
import scevo.tools.Options
import scevo.tools.OptionsFromArgs
import scevo.tools.TRandom
import scevo.tools.Rng
import scevo.evo.MultiobjectiveEvaluation

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
  def apply[T](s1: StatePop[T], s2: StatePop[T]) = new StatePop[T] {
    override val solutions = s1.solutions ++ s2.solutions
    override val iteration = math.max(s1.iteration, s2.iteration)
  }
}

// Function factories (component factories)

// Does this make sense? To reduce the number of parameters?
trait Environment extends Options with Collector
class EnvFromArgs(args: Array[String]) extends OptionsFromArgs(args) with CollectorFile with Environment
object EnvAndRng {
  def apply(args: Array[String]): (EnvFromArgs, TRandom) = {
    val env = new EnvFromArgs(args)
    (env, Rng(env))
  }
  def apply(args: String): (EnvFromArgs, TRandom) = apply(args.split("\\s+"))
}
object EnvWithRng {
  def apply(args: Array[String]) = {
    val env = new EnvFromArgs(args) {
      val rng = Rng(this)
    }
  }
}

object IterativeAlgorithm {
  def apply[S <: State](step: S => S)(stop: Seq[S => Boolean]): S => S = {
    @tailrec def iterate(s: S): S = stop.forall((sc: S => Boolean) => !sc(s)) match {
      case false => s
      case true  => iterate(step(s))
    }
    iterate
  }
  def apply[S <: Solution, E <: Evaluation](env: Environment)(step: StatePop[(S, E)] => StatePop[(S, E)])(stop: Seq[StatePop[(S, E)] => Boolean]): StatePop[(S, E)] => StatePop[(S, E)] = {
    val bsf = new BestSoFar[S, E]
    def reporting = bsf(env, env)
    apply(step andThen reporting)(stop) andThen EpilogueBestOfRun(bsf, env)
  }
}

// Evaluates population solution by solution (other modes of evaluation possible, e.g., in IFS)
object IndependentEval {
  def apply[S <: Solution, E <: Evaluation](f: S => E) =
    (s: StatePop[S]) => StatePop(s.solutions.map(x => (x, f(x))), s.iteration)
}

// Pulling parents implemented as stream; could be via iterator, but iterators are mutable
object Breeder {
  def apply[S <: Solution, E <: Evaluation](
    sel: Seq[(S, E)] => (S, E),
    solutionBuilder: () => (Stream[S] => (List[S], Stream[S])),
    isFeasible: S => Boolean = (_: S) => true) = {

    def selStream(src: Seq[(S, E)]): Stream[S] = sel(src)._1 #:: selStream(src)

    current: StatePop[(S, E)] => {
      val parentStream = selStream(current.solutions)
      @tailrec def breed(offspring: List[S], parStream: Stream[S]): Seq[S] =
        if (offspring.size >= current.solutions.size)
          offspring.take(current.solutions.size)
        else {
          val (off, parentTail) = solutionBuilder()(parStream)
          breed(offspring ++ off.filter(isFeasible), parentTail)
        }
      StatePop(breed(List[S](), parentStream), current.iteration + 1)
    }
  }
}

// Picks one of the functions (pipes) at random to 
object RandomMultiBreeder {
  def apply[S <: Solution](rng: TRandom, config: Options)(pipes: Seq[Stream[S] => (List[S], Stream[S])]) = {
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
    pop: Seq[(S, E)] => BestSelector(pop(rand, tournamentSize))
  }
}

object LexicaseSelection {
  def apply[S <: Solution, E <: MultiobjectiveEvaluation](opt: Options)(rand: TRandom) = {
    def sel(sols: Seq[(S, E)], cases: List[Int]): (S, E) =
      if (sols.size == 1)
        sols(0)
      else if (cases.size == 1)
        sols(rand)
      else {
        val theCase = cases(rand)
        val bestEval = BestSelector.select(sols.map(_._2.v(theCase)))
        //println("Sols:" + sols.size + " Cases: " + cases.size)
        sel(sols.filter(s => !bestEval.betterThan(s._2.v(theCase))), cases.diff(List(theCase)))
      }
    // assumes nonempty pop
    pop: Seq[(S, E)] => sel(pop, 0.until(pop(0)._2.size).toList)
  }
}

object RandomSelection {
  def apply[S <: Solution, E <: Evaluation](rand: TRandom) = {
    pop: Seq[(S, E)] => pop(rand)
  }
}
object RandomStatePop {
  def apply[S <: Solution](opt: Options, solutionGenerator: () => S) = {
    val populationSize = opt.paramInt("populationSize", 1000, _ > 0)
    _: Unit => StatePop(for (i <- 0 until populationSize) yield solutionGenerator())
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
  def apply[S <: Solution, E <: Evaluation](config: Options, otherCond: (S, E) => Boolean = (_: S, _: E) => false) = Seq(
    MaxIter[StatePop[(S, E)]](config),
    MaxTime(config),
    (s: StatePop[(S, E)]) => s.solutions.exists(es => otherCond(es._1, es._2)))
}

// Reporting
class BestSoFar[S <: Solution, E <: Evaluation] {
  protected var best: Option[(S, E)] = None
  def bestSoFar: Option[(S, E)] = best

  def apply(opt: Options, coll: Collector) = {
    val snapFreq = opt.paramInt("snapshot-frequency", 0)
    (s: StatePop[(S, E)]) => {
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
    (state: StatePop[(S, E)]) => {
      coll.rdb.setResult("lastGeneration", state.iteration)
      coll.rdb.setResult("bestOfRun.fitness", if (bsf.bestSoFar.isDefined) bsf.bestSoFar.get._2 else "NaN")
      coll.rdb.setResult("bestOfRun.genotype", bsf.bestSoFar.toString)
      coll.rdb.write("bestOfRun", bsf.bestSoFar)
      state
    }
}

object Experiment {
  def apply[S <: State](env: Environment)(alg: Unit => S) = {
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
/*
  def apply[S <: Solution, E <: Evaluation](solutionBuilder: () => (Seq[Tuple2[S, E]] => List[S]),
    targetSize: Int) = {
    current: StatePop[Tuple2[S, E]] =>
      @tailrec def breed(next: List[S]): Seq[S] =
        if (next.size >= targetSize)
          next.take(targetSize)
        else
          breed(solutionBuilder()(current.solutions) ++ next)
      StatePop[S](breed(List[S]()), current.iteration + 1)
  }
  * 
  */