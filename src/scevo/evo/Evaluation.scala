package scevo.evo

/**
  * Evaluation is any piece of information that results from an interaction of candidate solution with a problem.
  * In general, implements *partial* order.
  * Important: That order (whether complete or partial) is meant to represent the *objective*
  * relationships between the solutions.
  * The subjective ones can be expressed using search drivers.
  * E is the other Evaluation class that this evaluation should be comparable with.
  * Normally it is the same class.
  */
trait Evaluation[-E] extends Serializable {
  // Implements *partial* order: returns None in case of incomparability
  def comparePartial(that: E): Option[Int]
  def compareAny(that: Any) = comparePartial(that.asInstanceOf[E])
  def betterThan(that: E): Boolean = comparePartial(that).getOrElse(0) < 0
}

/**
  * Implements *complete* order
  *
  */
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
  override def comparePartial(that: ScalarEvaluation): Option[Int] = Some(compare(that))
}

class ScalarEvaluationMax(vv: Double)
    extends ScalarEvaluation(if (vv.isNaN) Double.MinValue else vv)
    with Evaluation[ScalarEvaluationMax] {
  override def betterThan(that: ScalarEvaluation): Boolean = v.compare(that.asInstanceOf[ScalarEvaluationMax].v) > 0
  override def compare(that: ScalarEvaluation) = that.asInstanceOf[ScalarEvaluationMax].v compare v
}
object ScalarEvaluationMax {
  def apply(v: Double) = new ScalarEvaluationMax(v)
}

class ScalarEvaluationMin(vv: Double)
    extends ScalarEvaluation(if (vv.isNaN) Double.MaxValue else vv)
    with Evaluation[ScalarEvaluationMin] {
  override def betterThan(that: ScalarEvaluation): Boolean = v.compare(that.asInstanceOf[ScalarEvaluationMin].v) < 0
  override def compare(that: ScalarEvaluation) = v compare that.asInstanceOf[ScalarEvaluationMin].v
}
object ScalarEvaluationMin {
  def apply(v: Double) = new ScalarEvaluationMin(v)
}

/**
  * Implements a vector of ScalarEvaluations
  *
  * The components of MultiobjectiveEvaluation must be ScalarEvaluations because they must obey complete orders
  * Note: Checks only if the other object has the same *number* of objectives;
  * does not check if the objectives are the same.
  */
class MultiobjectiveEvaluation(val v: Seq[ScalarEvaluation])
    extends Evaluation[MultiobjectiveEvaluation] {
  require(v.nonEmpty)
  override def toString = v.toString
  def apply(i: Int) = v(i)
  val size = v.size
  /* Not very elegant, but much faster than other method:
   */
  override def comparePartial(that: MultiobjectiveEvaluation): Option[Int] = {
    require(size == that.size)
    var meBetter: Boolean = false
    var thatBetter: Boolean = false
    for (i <- 0 until size) {
      val c = v(i).compareAny(that.v(i)).getOrElse(0)
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
    case other: MultiobjectiveEvaluation => size == other.size && (0 until size).forall(i => v(i) == other.v(i))
    case _                               => false
  }
  override def hashCode = (0 until size).map(i => v(i).hashCode).reduce(_ ^ _)
}

object MultiobjectiveEvaluation {
  def apply(v: Seq[ScalarEvaluation]) = new MultiobjectiveEvaluation(v)
}

/**
  * Stores multiple evaluations as a map, with the names serving as keys.
  *
  */
class MultiEvalNamed(val m: Map[Any, ScalarEvaluation])
    extends MultiobjectiveEvaluation(m.values.toVector)
    with Evaluation[MultiEvalNamed] {
  override def toString = m.toString
  def apply(k: Any) = m(k)
  override def comparePartial(that: MultiobjectiveEvaluation): Option[Int] = {
    if (m.keys == that.asInstanceOf[MultiEvalNamed].m.keys) super.comparePartial(that)
    else throw new IllegalArgumentException
  }
}

object MultiEvalNamed {
  def apply(m: Map[Any, ScalarEvaluation]) = new MultiEvalNamed(m)
}

class TestOutcomes(v: Seq[ScalarEvaluationMax]) extends MultiobjectiveEvaluation(v)
    with Evaluation[TestOutcomes] {
  require(v.forall(e => e.v >= 0 && e.v <= 1))
  def allPassed = v.forall(_.v == 1)
}

/* Overrides comparePartial(), comparing w.r.t. the number of tests passed.
 */
class BinaryTestOutcomes(v: Seq[ScalarEvaluationMax]) extends TestOutcomes(v)
    with Evaluation[BinaryTestOutcomes] {
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


