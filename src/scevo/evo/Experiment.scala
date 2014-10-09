package scevo.evo

import java.lang.management.ManagementFactory
import java.net.InetAddress
import java.net.UnknownHostException
import java.util.Calendar

import scevo.tools.OptionParser
import scevo.tools.Random
import scevo.tools.ResultDatabase

/*
 * Note that Experiment makes no reference to Solution types; only EvaluatedSolution
 */
abstract class Experiment[ES <: EvaluatedSolution[_ <: Evaluation]](args: Array[String]) {

  val options = OptionParser(args.toList)

  val seed = options.getOrElse("seed", "1").toInt
  val rng = new Random(seed)

  // General parameters read in from the command line
  val populationSize = options.getOrElse("populationSize", "1000").toInt
  assert(populationSize > 0, "Population size should be > 0")

  val maxGenerations = options.getOrElse("maxGenerations", "50").toInt
  assert(maxGenerations > 0, "Number of generations should be > 0")

  val maxTime = options.getOrElse("maxTime", "86400000").toLong
  assert(maxTime > 0, "MaxTime should be > 0")

  // TODO: 
  val operatorProbs = options.getOrElse("operatorProbs", "0.2,0.2,0.2,0.2,0.2").toString.split(",").map(_.toDouble).toList
  assert(operatorProbs.forall(_ >= 0), "Operator probs should be non-negative")
  assert(operatorProbs.sum == 1.0, "Operator probs should sum to 1.0")

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
  def stoppingConditions: List[StoppingCondition[ES]] = List(scMaxTime, scMaxGeneration)

  protected def run: Option[IterativeAlgorithm[ES]]

  def postGenerationCallback(state: IterativeAlgorithm[ES]): Unit =
    println(f"Generation: ${state.currentState.iteration}  BestSoFar: ${state.bestSoFar.eval}")

  def launch: Unit = {

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
      rdb.setResult("totalTimeSystem", scMaxTime.timeElapsed)
      rdb.setResult("system.endTime", Calendar.getInstance().getTime().toString)
      rdb.save
    }
  }

}



