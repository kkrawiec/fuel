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

trait Evaluator[S <: Solution, ES <: EvaluatedSolution[_]] extends Function1[Seq[S], Seq[ES]]



