package scevo.evo

import java.lang.management.ManagementFactory
import java.net.InetAddress
import java.net.UnknownHostException
import java.util.Calendar

import scevo.tools.ResultDatabase

/*
 * Note that Experiment makes no reference to Solution types; only EvaluatedSolution
 */

trait Experiment[S <: State] {
  this: Algorithm[S] with InitialState[S] with Options =>

  // Prepare result database and fill it with technical parameters of the experiment
  val rdb = new ResultDatabase("./")
  println("Result file: " + rdb.fname)
  options.foreach(t => rdb.put(t._1, t._2))
  try {
    rdb.setResult("system.hostname", InetAddress.getLocalHost().getHostName());
  } catch {
    case e: UnknownHostException => rdb.setResult("system.hostname", "could-not-determine");
  }
  rdb.setResult("system.java-version", System.getProperty("java.version"));
  rdb.setResult("system.pid", ManagementFactory.getRuntimeMXBean().getName());
  rdb.setResult("system.startTime", Calendar.getInstance().getTime().toString)

  rdb.put("mainClass", getClass.getName)
  rdb.put("status", "initialized")

  rdb.saveWorkingState

  def launch: Option[State] = {

    val startTime = System.currentTimeMillis()
    try {
      val state = run(initialState)
      rdb.setResult("lastGeneration", state.iteration)
      /*
      this match {
        case a: IterativeAlgorithm[_, _, _] => {
          rdb.setResult("bestOfRun.fitness", a.bestSoFar.eval)
          rdb.setResult("bestOfRun.genotype", a.bestSoFar.toString)
        }
      }
      * 
      */
      rdb.put("status", "completed")
      Some(state)
    } catch {
      case e: Exception => {
        rdb.put("status", "error: " + e.getLocalizedMessage + e.getStackTrace().mkString(" ")) // .toString.replace('\n', ' '))
        throw e
      }
    } finally {
      rdb.setResult("totalTimeSystem", System.currentTimeMillis() - startTime)
      rdb.setResult("system.endTime", Calendar.getInstance().getTime().toString)
      rdb.save
      None
    }
  }

}



