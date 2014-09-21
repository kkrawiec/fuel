package scevo.evo

/*
 * Represents a non-evaluated solution
 */

trait Solution

/*
 * Represents an evaluated solution. 
 * Only evaluated solutions can undergo selection. 
 * Search operators take evaluated solutions as arguments and produce non-evaluated solutions.
 */

trait EvaluatedSolution[+E <: Evaluation] {
  def eval: E
  //  final def betterThan(that: EvaluatedSolution[_ <: E]) = eval.betterThan(that.eval)
}

/*
 * Evaluation is any piece of information that results from an interaction 
 * of candidate solution with a task. 
 * Implements *partial* order, or no order at all
 */
trait Evaluation {
  // Implements *partial* order: returns None in case of incomparability
  def comparePartial(that: Evaluation): Option[Int]
  def betterThan(that: Evaluation): Boolean =
    comparePartial(that).getOrElse(0) > 0
}

// Implements *complete* order
abstract class ScalarEvaluation(val v: Double) extends Evaluation with Ordered[ScalarEvaluation] {
  override def betterThan(that: Evaluation): Boolean =
    compare(that.asInstanceOf[ScalarEvaluation]) > 0
  override def comparePartial(that: Evaluation): Option[Int] =
    Some(compare(that.asInstanceOf[ScalarEvaluation]))
  override def toString = v.toString
}

class ScalarEvaluationMax(override val v: Double) extends ScalarEvaluation(v) {
  override def compare(that: ScalarEvaluation) =
    math.signum(v - that.asInstanceOf[ScalarEvaluationMax].v).toInt
}
object ScalarEvaluationMax {
  def apply(v: Double) = new ScalarEvaluationMax(v)
}

class ScalarEvaluationMin(override val v: Double) extends ScalarEvaluation(v) {
  override def compare(that: ScalarEvaluation) =
    math.signum(that.asInstanceOf[ScalarEvaluationMin].v - v).toInt
}
object ScalarEvaluationMin {
  def apply(v: Double) = new ScalarEvaluationMin(v)
}

class MultiobjectiveEvaluation(val v: Seq[ScalarEvaluation]) extends Evaluation {
  override def toString = v.toString

  def comparePartial(that: Evaluation): Option[Int] = {
    val other = that.asInstanceOf[MultiobjectiveEvaluation]
    val n = v.length
    require(n == other.v.length)
    var xBetter: Int = 0
    var yBetter: Int = 0
    for (i <- 0 until n)
      if (v(i) > other.v(i)) // note: overloaded operator
        xBetter += 1
      else if (v(i) < other.v(i)) // note: overloaded operator
        yBetter += 1

    if (xBetter > 0)
      if (yBetter > 0)
        None
      else
        Some(1)
    else if (yBetter > 0)
      Some(-1)
    else
      Some(0)
  }
}
object MultiobjectiveEvaluation {
  def apply(v: Seq[ScalarEvaluation]) = new MultiobjectiveEvaluation(v)
}
