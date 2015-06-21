package scevo

import scevo.tools.TRandom
import scala.annotation.tailrec

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
    def apply(rnd: TRandom) = it.drop(rnd.nextInt(s.size)).next
  }
  /*
  implicit class RndApplyI[T](i: Iterable[T]) {
    def apply(rnd: TRandom) = i.drop( rnd.nextInt(i.size) ).next
  }
  * 
  */
}

// Histogram is a non-normalized Distribution
class Histogram(val d: Seq[Double]) {
  require(d.nonEmpty, "Histogram should contain at least one element")
  require(d.forall(_ >= 0), "Histogram elements should be non-negative")
  val sum = d.sum
  def apply(rng: TRandom): Int = {
    val r = sum * rng.nextDouble
    var theSum: Double = 0
    d.indexWhere(e => { theSum += e; theSum >= r })
  }
  // Multiple draws *without replacement*
  def applyOld(rng: TRandom, n: Int): Seq[Int] = {
    require(n <= d.size)
    @tailrec def draw(k: Int, dist: Histogram, remaining: Seq[Int], selected: List[Int]): Seq[Int] = k match {
      case 1 => remaining(dist(rng)) :: selected
      case _ => {
        val s = dist.apply(rng)
        val index = remaining(s)
        draw(k - 1, Histogram(dist.d.dropRight(dist.d.size - s) ++ dist.d.drop(s + 1)),
          remaining.dropRight(dist.d.size - s) ++ remaining.drop(s + 1), index :: selected)
      }
    }
    draw(n, this, 0.until(d.size).toSeq, List[Int]())
  }
  def apply(rng: TRandom, n: Int): Seq[Int] = {
    require(n <= d.size)
    @tailrec def draw(k: Int, s: Double, remaining: Set[Int], selected: List[Int]): Seq[Int] = k match {
      case 0 => selected
      case _ => {
        val r = s * rng.nextDouble
        var theSum: Double = 0
        val rem = remaining.toSeq
        val iter = rem.iterator
        var last = -1
        do {
          theSum += d(iter.next)
          last = last + 1
        } while (iter.hasNext && theSum < r)
        draw(k - 1, s - d(last), remaining.take(last) ++ iter.toSet, rem(last) :: selected)
      }
    }
    draw(n, sum, 0.until(d.size).toSet, List[Int]())
  }
}
object Histogram {
  def apply(d: Seq[Double]) = new Histogram(d)
}

class Distribution(d: Seq[Double]) extends Histogram(d) {
  // This was problematic due to numerical roundoffs
  // require(d.sum == 1.0, "Distribution should sum up to 1.0. And this one is: " + d)
  require(sum > 0.999 && sum < 1.001, "Distribution should sum up to 1.0. And this one is: " + d)
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
