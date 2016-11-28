package fuel.func

import java.util.Calendar
import fuel.util.Collector
import fuel.util.Options

/**
  * Deploys an algorithm and does the associated reporting.
  *
  */
object Experiment {
  def apply[S](alg: => S)(implicit opt: Options, coll: Collector): (Unit => Option[S]) = {
    _: Unit =>
      {
        coll.setResult("system.startTime", Calendar.getInstance().getTime().toString)
        val startTime = System.currentTimeMillis()
        try {
          val state = alg
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
          coll.rdb.save()
          opt.warnNonRetrieved
          if (opt('deleteOutputFile, true))
            coll.rdb.deleteArtifacts()
          None
        }
      }
  }
}


object RunExperiment {
  def apply[S](alg: Unit => S)(implicit opt: Options, coll: Collector) =
    Experiment(alg())(opt, coll)()
  def apply[S](alg: () => S)(implicit opt: Options, coll: Collector) =
    Experiment(alg())(opt, coll)()
}

