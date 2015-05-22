package scevo

import scevo.tools.TRandom

object Preamble {
  implicit class RndApply[T](s: Seq[T]) {
    require(s.nonEmpty)
    def apply(rnd: TRandom) = s(rnd.nextInt(s.size))
    // Note: with replacement!
    def apply(rnd: TRandom, n: Int) = for (i <- 0 until n) yield s(rnd.nextInt(s.size))
    def fapply(rnd: TRandom) = (() => s(rnd.nextInt(s.size)))
  }
  implicit class RndApplyS[T](s: Set[T]) {
    require(s.nonEmpty)
    val it = s.iterator
    def apply(rnd: TRandom) = it.drop( rnd.nextInt(s.size) ).next
  }
  /*
  implicit class RndApplyI[T](i: Iterable[T]) {
    def apply(rnd: TRandom) = i.drop( rnd.nextInt(i.size) ).next
  }
  * 
  */
}

class Distribution(val d: Seq[Double]) {
  require(d.nonEmpty, "Distribution should contain at least one element")
  // This was problematic due to numerical roundoffs
  // require(d.sum == 1.0, "Distribution should sum up to 1.0. And this one is: " + d)
  val s = d.sum
  require(d.sum > 0.999 && d.sum < 1.001, "Distribution should sum up to 1.0. And this one is: " + d)
  require(d.forall(_ >= 0), "Distribution elements should be non-negative")
  def apply(rng: TRandom): Int = {
    val r = rng.nextDouble
    var sum: Double = 0
    d.indexWhere(e => { sum += e; sum >= r })
  }
}

object Distribution {
  def apply(d: Seq[Double]) = new Distribution(d)
  def fromAnything(d: Seq[Double]) = new Distribution(d.map(_ / d.sum))
}

/*
  def fromAnything(d: Seq[Double]) = {
    // need this to circumvent numerical problems:
    val normalized = d.map(_ / d.sum).toList
    normalized.take(normalized.size-1)
    val last = 1.0 - normalized.take(normalized.size-1).sum
    new Distribution(normalized.tail)
  }
  * 
  */
