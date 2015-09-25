package scevo.func 

import java.util.Calendar
import scevo.evo.State

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


