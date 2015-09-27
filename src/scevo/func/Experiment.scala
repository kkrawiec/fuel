package scevo.func 

import java.util.Calendar
import scevo.evo.State
import scevo.tools.Options
import scevo.tools.Collector

object Experiment {
  def apply[S <: State](alg: Unit => S)(implicit opt: Options, coll: Collector) = {
    _: Unit =>
      {
        val startTime = System.currentTimeMillis()
        try {
          opt.warnNonRetrieved
          val state = alg()
          coll.set("status", "completed")
          if (opt.paramString("saveLastState", "false") == "true")
            coll.write("lastState", state)
          Some(state)
        } catch {
          case e: Exception => {
            coll.set("status", "error: " + e.getLocalizedMessage + e.getStackTrace().mkString(" ")) // .toString.replace('\n', ' '))
            throw e
          }
        } finally {
          coll.setResult("totalTimeSystem", System.currentTimeMillis() - startTime)
          coll.setResult("system.endTime", Calendar.getInstance().getTime().toString)
          coll.close
          None
        }
      }
  }
}


