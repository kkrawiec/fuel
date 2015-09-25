package scevo.func

import java.util.Calendar
import scala.annotation.tailrec
import scala.collection.immutable.Stream.consWrapper
import scala.collection.parallel.ForkJoinTaskSupport
import scevo.Distribution
import scevo.evo.BestSelector
import scevo.evo.State
import scevo.tools.Collector
import scevo.tools.Options
import scevo.tools.TRandom
import scevo.evo.BestES



// Reporting
class BestSoFar[S, E] {
  protected var best: Option[(S, E)] = None
  def bestSoFar: Option[(S, E)] = best

  def apply(opt: Options, coll: Collector, o: PartialOrdering[E]) = {
    val snapFreq = opt.paramInt("snapshot-frequency", 0)
    (s: StatePop[(S, E)]) => {
      val bestOfGen = BestES(s.solutions, o)
      if (bestSoFar.isEmpty || o.lt(bestOfGen._2,best.get._2)) best = Some(bestOfGen)
      println(f"Gen: ${s.iteration}  BestSoFar: ${bestSoFar.get}")
      if (snapFreq > 0 && s.iteration % snapFreq == 0)
        coll.saveSnapshot(f"${s.iteration}%04d")
      s
    }
  }
}

object EpilogueBestOfRun {
  def apply[S, E](bsf: BestSoFar[S, E], coll: Collector) =
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