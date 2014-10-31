package scevo.evo

import scevo.tools.OptionParser
import scevo.tools.Random
import scevo.tools.TRandom

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
  def rng: TRandom
}

trait Rng extends Randomness {
  this: Options =>
  val seed = options.getOrElse("seed", "1").toInt
  override lazy val rng = new Random(seed)
}

/*
 * Search operators. 
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
  lazy val searchOperators = operators.zip(operatorProbs)
}

trait Evaluator[S, ES] extends Function1[Seq[S], Seq[ES]]

trait PostIterationAction[ ES <: EvaluatedSolution[_ <: Evaluation]] {
  this: IterativeAlgorithm[ES] =>
  def postIteration: Unit =
    println(f"Generation: ${currentState.iteration}  BestSoFar: ${bestSoFar.eval}")
}

trait InitialState[S <: State] {
  def initialState: S
}