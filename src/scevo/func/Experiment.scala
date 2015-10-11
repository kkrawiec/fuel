package scevo.func

import java.util.Calendar
import scevo.evo.State
import scevo.tools.Options
import scevo.tools.Collector
import scevo.tools.TRandom
import scevo.tools.OptCollRng

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
          if (opt.paramString("saveLastState", "false") == "true")
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
          if (opt.paramBool("printResults"))
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

