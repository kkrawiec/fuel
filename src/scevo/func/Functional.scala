package scevo.func

import java.util.Calendar
import scala.annotation.tailrec
import scala.collection.immutable.Stream.consWrapper
import scevo.Distribution
import scevo.evo.BestSelector
import scevo.evo.Evaluation
import scevo.evo.Solution
import scevo.evo.State
import scevo.tools.Collector
import scevo.tools.Options
import scevo.tools.TRandom
import scala.collection.parallel.ForkJoinTaskSupport

/* Scevo-like functionality in functional programming style
 */

// Component factories

/* 
 * Note that IterativeAlgorithm is in general agnostic about evaluation.
 */
object IterativeAlgorithm {
  def apply[S <: State](step: S => S)(stop: Seq[S => Boolean]): S => S = {
    @tailrec def iterate(s: S): S = stop.forall((sc: S => Boolean) => !sc(s)) match {
      case false => s
      case true  => iterate(step(s))
    }
    iterate
  }
  // Version for populatin-based algorithms, 
  // with default best-so-far and best-of-run reporting
  def apply[S <: Solution, E <: Evaluation](
    env: Environment)(
      step: StatePop[(S, E)] => StatePop[(S, E)])(
        stop: Seq[StatePop[(S, E)] => Boolean]): StatePop[(S, E)] => StatePop[(S, E)] = {
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

object ParallelEval {
  def apply[S <: Solution, E <: Evaluation](f: S => E) = {
    (s: StatePop[S]) => StatePop(s.solutions.par.map(x => (x, f(x))).to, s.iteration)
  }
  def apply[S <: Solution, E <: Evaluation](f: S => E, parLevel: Int) = {
    val ts = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(parLevel))
    (s: StatePop[S]) => StatePop({
      val c = s.solutions.par
      c.tasksupport = ts
      c.map(x => (x, f(x))).to
    }, s.iteration)
  }
}

/* Pulling parents implemented as stream. 
 * Could be alternatively done via iterator, but iterators are mutable
*/
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

object RandomStatePop {
  def apply[S <: Solution](opt: Options, solutionGenerator: () => S) = {
    val populationSize = opt.paramInt("populationSize", 1000, _ > 0)
    _: Unit => StatePop(for (i <- 0 until populationSize) yield solutionGenerator())
  }
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
        coll.saveSnapshot(f"${s.iteration}%04d")
      s
    }
  }
}

object EpilogueBestOfRun {
  def apply[S <: Solution, E <: Evaluation](bsf: BestSoFar[S, E], coll: Collector) =
    (state: StatePop[(S, E)]) => {
      coll.setResult("lastGeneration", state.iteration)
      coll.setResult("bestOfRun.fitness", if (bsf.bestSoFar.isDefined) bsf.bestSoFar.get._2 else "NaN")
      coll.setResult("bestOfRun.genotype", bsf.bestSoFar.toString)
      coll.write("bestOfRun", bsf.bestSoFar)
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
          env.set("status", "completed")
          if (env.paramString("saveLastState", "false") == "true")
            env.write("lastState", state)
          Some(state)
        } catch {
          case e: Exception => {
            env.set("status", "error: " + e.getLocalizedMessage + e.getStackTrace().mkString(" ")) // .toString.replace('\n', ' '))
            throw e
          }
        } finally {
          env.setResult("totalTimeSystem", System.currentTimeMillis() - startTime)
          env.setResult("system.endTime", Calendar.getInstance().getTime().toString)
          env.close
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