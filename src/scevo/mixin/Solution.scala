package scevo.mixin

import scevo.evo.Evaluation

/*
 * Represents an evaluated solution. 
 * Only evaluated solutions can undergo selection. 
 * Search operators take evaluated solutions as arguments and produce non-evaluated solutions.
 */

trait EvaluatedSolution[S, +E <: Evaluation] extends Serializable {
  def s: S
  def eval: E
  override def toString = eval + " " + s
}

/* Default implementation of EvaluatedSolution
 * Using case class to have equals() for free
 */
case class ESol[S, +E <: Evaluation](override val s: S, override val eval: E)
  extends EvaluatedSolution[S, E]

