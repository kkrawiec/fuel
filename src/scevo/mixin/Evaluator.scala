package scevo.mixin

import scevo.mixin.ESol
import scevo.mixin.EvaluatedSolution
import scevo.evo.Evaluation

trait Evaluator[S, E <: Evaluation]
  extends Function1[Seq[S], Seq[EvaluatedSolution[S, E]]]

/* Default evaluator evaluates every solution separately, but this can be overriden
 */
trait SeparableEvaluator[S, E <: Evaluation]
  extends Evaluator[S, E] {
  def evaluate(s: S): E
  def apply(ss: Seq[S]) = ss.map(s => ESol(s, evaluate(s)))
}