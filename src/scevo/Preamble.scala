package scevo

import scevo.tools.TRandom

object Preamble {
  implicit def seq2rndApply[T](s: Seq[T]) = new RndApply[T](s)
}

class RndApply[T](s: Seq[T]) {
  require(s.nonEmpty)
  def apply(rnd: TRandom) = s(rnd.nextInt(s.size))
  def apply(rnd: TRandom, n: Int) = for (i <- 0 until n) yield s(rnd.nextInt(s.size))
}

class Distribution(val d: Seq[Double]) {
  require(d.sum == 1.0, "Distribution should sum up to 1.0")
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

//trait RandomSelection[T] extends Seq[(Double, T)]