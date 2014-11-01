package scevo.evo

import scevo.tools.Options


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
  val operatorProbs = options.getOrElse("operatorProbs", "0.2,0.2,0.2,0.2,0.2").toString.split(",").map(_.toDouble).toList
  assert(operatorProbs.forall(_ >= 0), "Operator probs should be non-negative")
  assert(operatorProbs.sum == 1.0, "Operator probs should sum to 1.0")
  lazy val searchOperators = operators.zip(operatorProbs)
}

trait Evaluator[S, ES] extends Function1[Seq[S], Seq[ES]]



