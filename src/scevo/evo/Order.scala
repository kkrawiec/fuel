package scevo.evo

trait MOrdering[E] extends PartialOrdering[Seq[E]] {
  def ordering(i: Int): Ordering[E]
  /* Not very elegant, but much faster than other method:
   */
  def lteq(x: Seq[E], y: Seq[E]): Boolean = tryCompare(x,y).getOrElse(0) < 0
  override def tryCompare(x: Seq[E], y: Seq[E]): Option[Int] = {
    require(x.size == y.size)
    var meBetter: Boolean = false
    var thatBetter: Boolean = false
    for (i <- 0 until x.size) {
      val c = ordering(i).compare(x(i), y(i))
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
}

/**
  * MulOrdering with the same ordering on all objectives
  *
  */
class MulOrderingSame[E](o: Ordering[E]) extends MOrdering[E] {
  override def ordering(i: Int) = o
}

class MulOrdering[E](val o: Seq[Ordering[E]]) extends MOrdering[E] {
  def ordering(i: Int): Ordering[E] = o(i)
  
}
