package scevo.mixin

import scevo.tools.Options
import scevo.Distribution
import scevo.tools.TRandom
import scevo.evo.Evaluation
import scevo.evo.Selector

/*
 * Search operators. 
 * 
 */
trait SearchOperators[S, E <: Evaluation] {
  def operators: Seq[Selector[S,E] => Seq[S]]
  assert(operators.nonEmpty, "At least one search operator should be declared")
}

trait StochasticSearchOperators[S, E <: Evaluation] extends SearchOperators[S, E] {
  this: Options =>
  val prob = paramString("operatorProbs")
  val distribution = Distribution(
    if (prob.isDefined)
      prob.get.split(",").map(_.toDouble)
    else {
      println("Probability distribution for operators undefined. Equal probabilities set.")
      val p = List.fill(operators.size - 1)(1.0 / operators.size)
      (1.0 - p.sum) :: p
    })
  require(distribution.d.size == operators.size, "Invalid number of operator probabilities")
  def operator(rng: TRandom) = operators(distribution(rng))
}






