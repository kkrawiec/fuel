package scevo.evo

import java.util.Calendar

import scevo.tools.Closeable
import scevo.tools.Collector
import scevo.tools.Options

trait Experiment[S <: State] extends Closeable {
  this: Algorithm[S] with Collector =>

  protected def runExperiment = run

  def launch: Option[State] = {
    val startTime = System.currentTimeMillis()
    try {
      val state = runExperiment
      rdb.put("status", "completed")
      rdb.write("lastState", state)
      Some(state)
    } catch {
      case e: Exception => {
        rdb.put("status", "error: " + e.getLocalizedMessage + e.getStackTrace().mkString(" ")) // .toString.replace('\n', ' '))
        throw e
      }
    } finally {
      close
      rdb.setResult("totalTimeSystem", System.currentTimeMillis() - startTime)
      rdb.setResult("system.endTime", Calendar.getInstance().getTime().toString)
      None
    }
  }
}


