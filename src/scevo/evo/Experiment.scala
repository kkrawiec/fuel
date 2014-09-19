package scevo.evo

import scevo.tools.ResultDatabase
import scevo.tools.Random
import java.net.UnknownHostException
import java.util.Calendar
import java.lang.management.ManagementFactory
import java.net.InetAddress
import scevo.tools.OptionParser

/*
 * Note that Experiment makes no reference to Solution types; only EvaluatedSolution
 */
abstract class Experiment[ES <: EvaluatedSolution[_ <: Evaluation]](args: Array[String]) {

  val options = OptionParser(args.toList)

  val seed = options("seed").toInt
  val rng = new Random(seed)

  // General parameters read in from the command line

  val populationSize = options("populationSize").toInt
  assert(populationSize > 0, "Population size should be > 0")

  val maxGenerations = options("maxGenerations").toInt
  assert(maxGenerations > 0, "Number of generations should be > 0")

  val maxTime = options("maxTime").toLong
  assert(maxTime > 0, "MaxTime should be > 0")

  val operatorProbs = options("operatorProbs").toString.split(",").map(_.toDouble).toList
  assert(maxGenerations > 0, "Number of generations should be > 0")

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

  val scMaxTime = new MaxTime[ES](maxTime)
  val scMaxGeneration = new MaxGenerations[ES](maxGenerations)
  def stoppingConditions : List[StoppingCondition[ES]] = List( scMaxTime, scMaxGeneration )

  protected def run: IterativeAlgorithm[ES]

   def postGenerationCallback(state: IterativeAlgorithm[ES]): Unit = 
        println(f"Generation: ${state.currentState.iteration}  BestSoFar: ${state.bestSoFar.eval}")

  def launch: Unit = {

    try {
      val alg  = run

      rdb.setResult("lastGeneration", alg.currentState.iteration )
      rdb.setResult("bestOfRun.fitness", alg.bestSoFar.eval )
      rdb.setResult("bestOfRun.genotype", alg.bestSoFar.toString() )
      rdb.put("status", "completed")
    } catch {
      case e: Exception => {
        rdb.put("status", "error: " + e.getLocalizedMessage + e.getStackTrace().mkString(" ")) // .toString.replace('\n', ' '))
        throw e
      }
    } finally {
      rdb.setResult("totalTimeSystem", scMaxTime.timeElapsed)
      rdb.setResult("system.endTime", Calendar.getInstance().getTime().toString)
      rdb.save
    }
  }

}



