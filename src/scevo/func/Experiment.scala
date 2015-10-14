package scevo.func

import java.util.Calendar

import scevo.core.State
import scevo.util.Collector
import scevo.util.Options

/**
  * Deploys an algorithm and does the associated reporting.
  *
  */
object Experiment {
  def apply[S <: State](alg: Unit => S)(implicit opt: Options, coll: Collector): (Unit => Option[S]) = {
    _: Unit =>
      {
        coll.setResult("system.startTime", Calendar.getInstance().getTime().toString)
        val startTime = System.currentTimeMillis()
        try {
          val state = alg()
          coll.set("status", "completed")
          if (opt('saveLastState, false))
            coll.write("lastState", state)
          Some(state)
        } catch {
          case e: Exception =>
            {
              coll.set("status", "error: " + e.getLocalizedMessage + e.getStackTrace().mkString(" ")) // .toString.replace('\n', ' '))
              throw e
            }
            None
        } finally {
          coll.setResult("totalTimeSystem", System.currentTimeMillis() - startTime)
          coll.setResult("system.endTime", Calendar.getInstance().getTime().toString)
          if (opt('printResults, false))
            println(coll.rdb.toString)
          coll.close
          opt.warnNonRetrieved
          None
        }
      }
  }
}

object RunExperiment {
  def apply[S <: State](alg: Unit => S)(implicit opt: Options, coll: Collector) =
    Experiment(alg)(opt, coll)()
}

