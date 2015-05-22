package scevo.evo

import org.junit.Test

trait Evaluator[S <: Solution, E <: Evaluation]
  extends Function1[Seq[S], Seq[EvaluatedSolution[S, E]]] 

/* Default evaluator evaluates every solution separately, but this can be overriden
 */
trait SeparableEvaluator[S <: Solution, E <: Evaluation]
  extends Evaluator[S,E] {
  def evaluate(s: S): E
  def apply(ss: Seq[S]) = ss.map(s => ESol(s, evaluate(s)))
}

/* Evaluation is any piece of information that results from an interaction 
 * of candidate solution with a task. 
 * In general, implements *partial* order, or no order at all. 
 * Important: That order (whether complete or partial) is meant to represent the *objective*
 * relationships between the solutions. 
 * The subjective ones can be expressed using search drivers. 
 * Of course, most often the search driver used to guide the search will be simply 
 * the objective ordering/function. 
 */

trait Evaluation extends Serializable {
  // Implements *partial* order: returns None in case of incomparability
  def comparePartial(that: Evaluation): Option[Int]
  def betterThan(that: Evaluation): Boolean =
    comparePartial(that).getOrElse(0) < 0
}

// TODO: Introduce trait LinearOrder

// Implements *complete* order
abstract class ScalarEvaluation(val v: Double)
  extends Evaluation with Ordered[ScalarEvaluation] {
  require(!v.isNaN, "ScalarEvalution cannot be NaN")
  override def betterThan(that: Evaluation): Boolean =
    compare(that.asInstanceOf[ScalarEvaluation]) < 0
  override def comparePartial(that: Evaluation): Option[Int] =
    Some(compare(that.asInstanceOf[ScalarEvaluation]))
  override def toString = v.toString
  override def equals(that: Any) = that match {
    case other: ScalarEvaluation => v == other.v
    case _                       => false
  }
  override def hashCode = v.hashCode
}

class ScalarEvaluationMax(vv: Double)
  extends ScalarEvaluation(if (vv.isNaN) Double.MinValue else vv) {
  override def compare(that: ScalarEvaluation) = that.asInstanceOf[ScalarEvaluationMax].v compare v
}
object ScalarEvaluationMax {
  def apply(v: Double) = new ScalarEvaluationMax(v)
}

class ScalarEvaluationMin(vv: Double)
  extends ScalarEvaluation(if (vv.isNaN) Double.MaxValue else vv) {
  override def compare(that: ScalarEvaluation) = v compare that.asInstanceOf[ScalarEvaluationMin].v
}
object ScalarEvaluationMin {
  def apply(v: Double) = new ScalarEvaluationMin(v)
}

class MultiobjectiveEvaluation(val v: Seq[ScalarEvaluation]) extends Evaluation {
  require(v.nonEmpty)
  override def toString = v.toString
  /* Not very elegant, but much faster than other method I've considered:
   */
  override def comparePartial(that: Evaluation): Option[Int] = {
    val other = that.asInstanceOf[MultiobjectiveEvaluation]
    val n = v.length
    require(n == other.v.length)
    var meBetter: Boolean = false
    var thatBetter: Boolean = false
    for (i <- 0 until n) {
      val c = v(i) compare other(i)
      if (c < 0)
        meBetter = true
      else if (c > 0)
        thatBetter = true
    }
    (meBetter, thatBetter) match {
      case (true, true)  => None
      case (true, false) => Some(-1) // BUG!!! Was: 1
      case (false, true) => Some(1)
      case _             => Some(0)
    }
  }
  def apply(i:Int) = v(i)
  def size = v.size
  override def equals(that: Any) = that match {
    case other: MultiobjectiveEvaluation => v.size == other.v.size && (0 until v.size).forall(i => v(i) == other.v(i))
    case _                               => false
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
/* Important: Overrides comparePartial(), comparing w.r.t. the number of tests passed.
 */
class BinaryTestOutcomes(override val v: Seq[ScalarEvaluationMax]) extends TestOutcomes(v) {
  require(v.forall(e => e.v == 0 || e.v == 1))
  override def toString = numTestsPassed.toString // + super.toString
  def numTestsPassed = v.map(_.v).sum
  override def comparePartial(that: Evaluation): Option[Int] =
    Some(that.asInstanceOf[BinaryTestOutcomes].numTestsPassed compare numTestsPassed)
}

// Lexicographic ascending order, e minimized
class EvalLexMin(val e: Double*) extends Evaluation {
  require(e.nonEmpty)
  def comparePartial(that: Evaluation): Option[Int] = {
    val other = that.asInstanceOf[EvalLexMin]
    //require(e.size == other.e.size) 
    val r = e.zip(other.e).find({ case (me, oth) => me != oth }).getOrElse((0.0, 0.0))
    Some((r._2 - r._1).toInt)
  }
}

final class TestEvaluation {
  @Test
  def test: Unit = {
    val x = ScalarEvaluationMax(3.0)
    val y = ScalarEvaluationMax(3.0)
    val z = ScalarEvaluationMax(2.0)
    println(x equals y)
    println(x compare y)
    println(y compare x)
    println(x compare z)
    println(z compare x)
    
    val m0 = MultiobjectiveEvaluation(Seq(x,x))
    val m1 = MultiobjectiveEvaluation(Seq(x,z))
    val m2 = MultiobjectiveEvaluation(Seq(z,x))
    val m3 = MultiobjectiveEvaluation(Seq(z,z))
    println(m1 comparePartial m1)
    println(m1 comparePartial m2)
    println(m1 comparePartial m3)
    println(m3 comparePartial m1)

    println(m1 betterThan m1)
    println(m1 betterThan m2)
    println(m1 betterThan m3)
    println(m3 betterThan m1)

    println(m0 betterThan m3)
  }
}