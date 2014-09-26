package scevo

import scevo.tools.TRandom

// Worth considering: global random number generator:
// object rng extends Random(1)

object Preamble {
  implicit def seq2rndApply[T](s: Seq[T]) = new RndApply[T](s)
}

class RndApply[T](s: Seq[T]) {
  require(s.nonEmpty)
  def apply(rnd: TRandom) = s(rnd.nextInt(s.size))
  def apply(rnd: TRandom, n: Int) = for (i <- 0 until n) yield s(rnd.nextInt(s.size))
}