package scevo.core

/**
  * Dominance relation, a special case of PartialOrder, where objects are compared
  * along a number of objectives.
  *
  * Depending on the interpretation of the Some(0) result, can be used as
  * strong dominance or weak dominance.
  */
trait Dominance[E] extends PartialOrdering[Seq[E]] {
  def ordering(i: Int): Ordering[E]
  def lteq(x: Seq[E], y: Seq[E]): Boolean = tryCompare(x, y).getOrElse(0) < 0
  override def tryCompare(x: Seq[E], y: Seq[E]): Option[Int] = {
    require(x.size == y.size)
    // Not very elegant, but much faster than other method:
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

object Best {
  def apply[T](s: Seq[T])(implicit o: PartialOrdering[T]) = {
    if (s.isEmpty) None
    else {
      // First pass: find the element (l) that is <= than all elements 
      // at the subsequent positions
      var b = s.head
      var l = 0
      for (i <- 1 until s.size) {
        val c = o.tryCompare(s(i), b)
        if (c.nonEmpty) {
          if (c.get < 0) {
            b = s(i)
            l = i
          }
        } else l = -1
      }
      // Second pass: check if that element is also <= than all elements
      // at positions from 0 to l
      if (l < 0) None
      else {
        var i = -1
        var r: Option[Int] = None
        do {
          i = i + 1
          r = o.tryCompare(s(i), b)
        } while (i < l && r.nonEmpty && r.get >= 0)
        if (i == l) Some(b)
        else None
      }
    }
  }
}

object TestBest extends App {
  val a = Seq(1, 1)
  val b = Seq(2, 2)
  val c = Seq(1, 3)
  val d = Seq(3, 1)
  val e = Seq(2, 2)

  implicit val o = Dominance[Int]

  for (p <- Seq(a, b, c, d, e).permutations)
    assert(Best(p) == Some(a))
  for (p <- Seq(b, c, d).permutations)
    assert(Best(p) == None)
  for (p <- Seq(b, c, d, e).permutations)
    assert(Best(p) == None)
  for (p <- Seq(a, b, c, d, e))
    assert(Best(Seq(p)) == Some(p))
}