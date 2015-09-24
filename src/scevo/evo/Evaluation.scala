package scevo.evo

/* Evaluation is any piece of information that results from an interaction 
 * of candidate solution with a problem. 
 * In general, implements *partial* order.
 * Important: That order (whether complete or partial) is meant to represent the *objective*
 * relationships between the solutions. 
 * The subjective ones can be expressed using search drivers. 
 */

/* E is the other Evaluation class that this evaluation should be comparable with. 
 * Normally it is the same class.
 */
trait Evaluation[E] extends Serializable {
  // Implements *partial* order: returns None in case of incomparability
  def comparePartial(that: E): Option[Int]
  def compareAny(that: Any) = comparePartial(that.asInstanceOf[E])
  def betterThan(that: E): Boolean = comparePartial(that).getOrElse(0) < 0
}

object Eval {
  implicit class comparePartial[E1 <: Evaluation](a: )
}
// Implements *complete* order
abstract class ScalarEvaluation(val v: Double)
    extends Evaluation[ScalarEvaluation]
    with Ordered[ScalarEvaluation] {
  require(!v.isNaN, "ScalarEvalution cannot be NaN")
  override def toString = v.toString
  override def equals(that: Any) = that match {
    case other: ScalarEvaluation => v == other.v
    case _                       => false
  }
  override def hashCode = v.hashCode
}

class ScalarEvaluationMax(vv: Double)
    extends ScalarEvaluation(if (vv.isNaN) Double.MinValue else vv) {
  override def betterThan(that: ScalarEvaluation): Boolean = v.compare(that.v) < 0
  override def compare(that: ScalarEvaluation) = that.v compare v
  override def comparePartial(that: ScalarEvaluationMax): Option[Int] = Some(compare(that))
}
object ScalarEvaluationMax {
  def apply(v: Double) = new ScalarEvaluationMax(v)
}

class ScalarEvaluationMin(vv: Double)
    extends ScalarEvaluation(if (vv.isNaN) Double.MaxValue else vv) {
  override def betterThan(that: ScalarEvaluationMin): Boolean = compare(that) < 0
  override def compare(that: ScalarEvaluationMin) = v compare that.v
  override def comparePartial(that: ScalarEvaluationMin): Option[Int] = Some(compare(that))
}
object ScalarEvaluationMin {
  def apply(v: Double) = new ScalarEvaluationMin(v)
}

/* The components of MultiobjectiveEvaluation must be ScalarEvaluations because they must obey complete orders 
 * Note: Checks only if the other object has the same *number* of objectives; 
 * does not check if the objectives are the same.  
 */

trait AbstractMultiobjectiveEvaluation {
  def apply(i: Int): Evaluation[_]
  def size: Int
  /* Not very elegant, but much faster than other method:
   */
  override def comparePartial(that: AbstractMultiobjectiveEvaluation): Option[Int] = {
    val n = size
    require(n == that.size)
    var meBetter: Boolean = false
    var thatBetter: Boolean = false
    for (i <- 0 until n) {
      val c = apply(i).compareAny(that(i)).getOrElse(0)
      if (c < 0)
        meBetter = true
      else if (c > 0)
        thatBetter = true
    }
    (meBetter, thatBetter) match {
      case (true, true)  => None
      case (true, false) => Some(-1)
      case (false, true) => Some(1)
      case _             => Some(0)
    }
  }
  override def equals(that: Any) = that match {
    case other: AbstractMultiobjectiveEvaluation => size == other.size && (0 until size).forall(i => apply(i) == other(i))
    case _                                       => false
  }
  override def hashCode = (0 until size).map(i => apply(i).hashCode).reduce(_ ^ _)
}

class MultiobjectiveEvaluation(val v: Seq[Evaluation[_]])
    extends Evaluation[MultiobjectiveEvaluation] with AbstractMultiobjectiveEvaluation {
  require(v.nonEmpty)
  override def toString = v.toString
  override def apply(i: Int) = v(i)
  override def size = v.size
}

object MultiobjectiveEvaluation {
  def apply[E](v: Seq[Evaluation[E]]) = new MultiobjectiveEvaluation(v)
}

// TODO: equals
class MultiEvalNamed(val m: Map[Any, Evaluation[_]])
    extends MultiobjectiveEvaluation(m.values.toVector) {
  override def toString = m.toString
  def apply(k: Any) = m(k)
}

object MultiEvalNamed {
  def apply(m: Map[Any, Evaluation[_]]) = new MultiEvalNamed(m)
}

class TestOutcomes(override val v: Seq[ScalarEvaluationMax]) extends MultiobjectiveEvaluation(v) {
  require(v.forall(e => e.v >= 0 && e.v <= 1))
  def allPassed = v.forall(_.v == 1)
  override def apply(i: Int): ScalarEvaluationMax = v(i)
}
/* Important: Overrides comparePartial(), comparing w.r.t. the number of tests passed.
 */
class BinaryTestOutcomes(override val v: Seq[ScalarEvaluationMax]) extends TestOutcomes(v) {
  require(v.forall(e => e.v == 0 || e.v == 1))
  override def toString = numTestsPassed.toString // + super.toString
  def numTestsPassed = v.map(_.v).sum
  override def comparePartial(that: MultiobjectiveEvaluation): Option[Int] =
    Some(that.asInstanceOf[BinaryTestOutcomes].numTestsPassed compare numTestsPassed)
}

// Lexicographic ascending order, e minimized
class EvalLexMin(val e: Double*) extends Evaluation[EvalLexMin] {
  require(e.nonEmpty)
  def comparePartial(that: EvalLexMin): Option[Int] = {
    //require(e.size == other.e.size) 
    val r = e.zip(that.e).find({ case (me, oth) => me != oth }).getOrElse((0.0, 0.0))
    Some((r._2 - r._1).toInt)
  }
}


