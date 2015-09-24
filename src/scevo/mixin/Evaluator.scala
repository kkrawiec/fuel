package scevo.mixin

import scevo.evo.Evaluation

trait Evaluator[S, E <: Evaluation[_]]
  extends Function1[Seq[S], Seq[EvaluatedSolution[S, E]]]

/* Default evaluator evaluates every solution separately, but this can be overriden
 */
trait SeparableEvaluator[S, E <: Evaluation[_]]
  extends Evaluator[S, E] {
  def evaluate(s: S): E
  def apply(ss: Seq[S]) = ss.map(s => ESol(s, evaluate(s)))
}