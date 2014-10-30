package scevo.evo

import java.lang.management.ManagementFactory
import java.net.InetAddress
import java.net.UnknownHostException
import java.util.Calendar

import scevo.tools.ResultDatabase

/*
 * Note that Experiment makes no reference to Solution types; only EvaluatedSolution
 */

trait Experiment[ES <: EvaluatedSolution[_ <: Evaluation]] {
  this: Options  =>

  protected def run: Option[IterativeAlgorithm[ES]]

  // General parameters read in from the command line
  val populationSize = options.getOrElse("populationSize", "1000").toInt
  assert(populationSize > 0, "Population size should be > 0")

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

  def postGenerationCallback(state: IterativeAlgorithm[ES]): Unit =
    println(f"Generation: ${state.currentState.iteration}  BestSoFar: ${state.bestSoFar.eval}")

  def launch: Unit = {

    val startTime = System.currentTimeMillis()
    try {
      val res = run
      if (res.isDefined) {
        val alg = res.get
        rdb.setResult("lastGeneration", alg.currentState.iteration)
        rdb.setResult("bestOfRun.fitness", alg.bestSoFar.eval)
        rdb.setResult("bestOfRun.genotype", alg.bestSoFar.toString())
      }
      rdb.put("status", "completed")
    } catch {
      case e: Exception => {
        rdb.put("status", "error: " + e.getLocalizedMessage + e.getStackTrace().mkString(" ")) // .toString.replace('\n', ' '))
        throw e
      }
    } finally {
      rdb.setResult("totalTimeSystem", System.currentTimeMillis() - startTime)
      rdb.setResult("system.endTime", Calendar.getInstance().getTime().toString)
      rdb.save
    }
  }

}



