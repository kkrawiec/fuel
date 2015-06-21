package scevo.evo

import java.util.Calendar

import scevo.tools.Closeable
import scevo.tools.Collector
import scevo.tools.Options

trait Experiment[S <: State] extends Closeable {
  this: Algorithm[S] with Collector with Options =>

  protected def runExperiment = run

  def launch: Option[S] = {
    val startTime = System.currentTimeMillis()
    try {
      warnNonRetrieved
      val state = runExperiment
      rdb.put("status", "completed")
      if (getOption("saveLastState", "false") == "true")
        rdb.write("lastState", state)
      Some(state)
    } catch {
      case e: Exception => {
        rdb.put("status", "error: " + e.getLocalizedMessage + e.getStackTrace().mkString(" ")) // .toString.replace('\n', ' '))
        throw e
      }
    } finally {
      rdb.setResult("totalTimeSystem", System.currentTimeMillis() - startTime)
      rdb.setResult("system.endTime", Calendar.getInstance().getTime().toString)
      close
      None
    }
  }
}


