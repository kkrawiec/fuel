package scevo.evo

import scevo.tools.Options
import scevo.Distribution
import scevo.tools.TRandom

/*
 * Search operators. 
 * 
 */
trait SearchOperators[S <: Solution, E <: Evaluation] {
  def operators: Seq[Selector[S,E] => Seq[S]]
  assert(operators.nonEmpty, "At least one search operator should be declared")
}

trait StochasticSearchOperators[S <: Solution, E <: Evaluation] extends SearchOperators[S, E] {
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



trait Evaluator[S <: Solution, E <: Evaluation]
  extends Function1[Seq[S], Seq[EvaluatedSolution[S, E]]] 

/* Default evaluator evaluates every solution separately, but this can be overriden
 */
trait SeparableEvalutator[S <: Solution, E <: Evaluation]
  extends Evaluator[S,E] {
  def evaluate(s: S): E
  def apply(ss: Seq[S]) = ss.map(s => ESol(s, evaluate(s)))
}



