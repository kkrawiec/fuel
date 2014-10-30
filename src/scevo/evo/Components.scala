package scevo.evo

import scevo.tools.OptionParser
import scevo.tools.Random

/*
 * The basic components to be then combined via mixins. 
 */

/*
 * Generic option/parameter provider. 
 * TODO: Consider renaming to Parameters
 */
trait Options {
  def options: Map[String, String]
}

abstract class OptionsFromArgs(args: Array[String]) extends Options {
  override lazy val options = OptionParser(args.toList)
}

/*
 * Generic randomness provider. 
 */
trait Randomness {
  def rng: Random
}

trait Rng extends Randomness {
  this: Options =>
  val seed = options.getOrElse("seed", "1").toInt
  override lazy val rng = new Random(seed)
}

/*
 * Stopping conditions. 
 */
trait StoppingConditions[ES <: EvaluatedSolution[_ <: Evaluation]] {
  def stoppingConditions: List[StoppingCondition[ES]]
}

trait StoppingStd[ES <: EvaluatedSolution[_ <: Evaluation]] extends StoppingConditions[ES] {
  this: Options =>

  val maxGenerations = options.getOrElse("maxGenerations", "50").toInt
  assert(maxGenerations > 0, "Number of generations should be > 0")

  val maxTime = options.getOrElse("maxTime", "86400000").toLong
  assert(maxTime > 0, "MaxTime should be > 0")

  val scMaxTime = new MaxTime[ES](maxTime)
  val scMaxGeneration = new MaxGenerations[ES](maxGenerations)

  def stoppingConditions = List(scMaxTime, scMaxGeneration)
}

/* Search operators. 
 * 
 */
trait SearchOperators[ES <: EvaluatedSolution[E], E <: Evaluation, S <: Solution] {
  def operators: Seq[Selector[ES, E] => Seq[S]]
}

trait StochasticSearchOperators[ES <: EvaluatedSolution[E], E <: Evaluation, S <: Solution] extends SearchOperators[ES, E, S] {
  this: Options =>
  val operatorProbs = options.getOrElse("operatorProbs", "0.2,0.2,0.2,0.2,0.2").toString.split(",").map(_.toDouble).toList
  assert(operatorProbs.forall(_ >= 0), "Operator probs should be non-negative")
  assert(operatorProbs.sum == 1.0, "Operator probs should sum to 1.0")
}
 
