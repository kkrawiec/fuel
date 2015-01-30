package scevo.evo

/*
 * Represents a non-evaluated solution
 */

trait Solution extends Serializable

/*
 * Represents an evaluated solution. 
 * Only evaluated solutions can undergo selection. 
 * Search operators take evaluated solutions as arguments and produce non-evaluated solutions.
 */

trait EvaluatedSolution[S <: Solution, +E <: Evaluation] extends Serializable {
  def s: S
  def eval: E
  override def toString = eval + " " + s
}

/* Default implementation of EvaluatedSolution
 * Using case class to have equals() for free
 */
case class ESol[S <: Solution, +E <: Evaluation](override val s: S, override val eval: E)
  extends EvaluatedSolution[S, E]
/*
object ESol {
  def apply[S <: Solution, E <: Evaluation](s: S, eval: E) = new ESol(s, eval)
}
* 
*/
