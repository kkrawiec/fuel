package scevo.evo

import scevo.tools.Options
import scevo.Distribution
import scevo.tools.TRandom


/*
 * Search operators. 
 * 
 */
trait SearchOperators[ES <: EvaluatedSolution[_], S <: Solution] {
  def operators: Seq[Selector[ES] => Seq[S]]
  assert(operators.nonEmpty, "At least one search operator should be declared")
}

trait StochasticSearchOperators[ES <: EvaluatedSolution[_], S <: Solution] extends SearchOperators[ES, S] {
  this: Options =>
  val distribution = Distribution(options.getOrElse("operatorProbs", "0.2,0.2,0.2,0.2,0.2")
  .split(",").map(_.toDouble).toList)
  require(distribution.d.size == operators.size, "Invalid number of operator probabilities")
//  lazy val searchOperators = operators.zip(operatorProbs)
  def operator(rng: TRandom) = operators(distribution(rng))

}

trait Evaluator[S, ES] extends Function1[Seq[S], Seq[ES]]



