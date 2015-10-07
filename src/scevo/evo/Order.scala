package scevo.evo

trait Dominance[E] extends PartialOrdering[Seq[E]] {
  def ordering(i: Int): Ordering[E]
  /* Not very elegant, but much faster than other method:
   */
  def lteq(x: Seq[E], y: Seq[E]): Boolean = tryCompare(x, y).getOrElse(0) < 0
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

object Dominance {
  /**
    * Dominance with the same ordering on all objectives
    */
  def apply[E](implicit o: Ordering[E]) = new Dominance[E] {
    override def ordering(i: Int) = o
  }
  /**
    * Dominance with different orderings on particular objectives
    */
  def apply[E](o: Seq[Ordering[E]]) = new Dominance[E] {
    override def ordering(i: Int): Ordering[E] = o(i)
  }
}
