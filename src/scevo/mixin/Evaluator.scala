package scevo.mixin

import scevo.evo.ESol
import scevo.evo.EvaluatedSolution
import scevo.evo.Evaluation
import scevo.evo.Solution

trait Evaluator[S <: Solution, E <: Evaluation]
  extends Function1[Seq[S], Seq[EvaluatedSolution[S, E]]]

/* Default evaluator evaluates every solution separately, but this can be overriden
 */
trait SeparableEvaluator[S <: Solution, E <: Evaluation]
  extends Evaluator[S, E] {
  def evaluate(s: S): E
  def apply(ss: Seq[S]) = ss.map(s => ESol(s, evaluate(s)))
}