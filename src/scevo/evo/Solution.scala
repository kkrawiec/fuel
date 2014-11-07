package scevo.evo

import org.junit.Test

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
  override def equals(that: Any) = that match {
    case other: ScalarEvaluation => v == other.v
    case _ => false
  }
  override def hashCode = v.hashCode
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
  require(v.nonEmpty)
  override def toString = v.toString
  /*
   * Not very elegant, but much faster:
   */
  def comparePartial(that: Evaluation): Option[Int] = {
    val other = that.asInstanceOf[MultiobjectiveEvaluation]
    val n = v.length
    require(n == other.v.length)
    var xBetter: Boolean = false
    var yBetter: Boolean = false
    for (i <- 0 until n) {
      val c = v(i).compare(other.v(i))
      if (c > 0)
        xBetter = true
      else if (c < 0)
        yBetter = true
    }
    (xBetter, yBetter) match {
      case (true, true) => None
      case (true, false) => Some(1)
      case (false, true) => Some(-1)
      case _ => Some(0)
    }
  }
  override def equals(that: Any) = that match {
    case other: MultiobjectiveEvaluation => v.size == other.v.size && (0 until v.size).forall(i => v(i) == other.v(i))
    case _ => false
  }
  override def hashCode = v.map(_.hashCode).reduce(_ ^ _)
}

object MultiobjectiveEvaluation {
  def apply(v: Seq[ScalarEvaluation]) = new MultiobjectiveEvaluation(v)
}

class TestOutcomes(override val v: Seq[ScalarEvaluationMax]) extends MultiobjectiveEvaluation(v) {
  require(v.forall(e => e.v >= 0 && e.v <= 1))
  def allPassed = v.forall(_.v == 1)
}
class BinaryTestOutcomes(override val v: Seq[ScalarEvaluationMax]) extends TestOutcomes(v) {
  require(v.forall(e => e.v == 0 || e.v == 1))
//  override def toString = s"Passed: ${v.map(_.v).sum} " + super.toString
}

final class TestEvaluation {
  @Test
  def test: Unit = {
    val x = ScalarEvaluationMin(3.0)
    val y = ScalarEvaluationMin(3.0)
    println(x equals y)
  }
}
