package scevo

import scevo.util.TRandom
import scala.annotation.tailrec

object Preamble {
  /** Draws uniformly a single element from the sequence */
  implicit class RndApply[T](s: Seq[T]) {
    assume(s.nonEmpty)
    def apply(rnd: TRandom) = s(rnd.nextInt(s.size))
    // Note: with replacement!
    def apply(rnd: TRandom, n: Int) = for (i <- 0 until n) yield s(rnd.nextInt(s.size))
    def fapply(rnd: TRandom) = (() => s(rnd.nextInt(s.size)))
  }
  implicit class RndApplyS[T](s: Set[T]) {
    assume(s.nonEmpty)
    val it = s.iterator
    def apply(rnd: TRandom) = it.drop(rnd.nextInt(s.size)).next
  }
  /** Iverson's bracket */
  implicit def iverson(b:Boolean) = if (b) 1 else 0
}

/** Histogram is basically a non-normalized Distribution */
class Histogram[T](val d: Seq[T])(implicit num: Numeric[T]) {
  assume(d.nonEmpty, "Histogram should contain at least one element")
  assume(d.forall(e => num.gteq(e, num.zero)), "Histogram elements should be non-negative")
  val sum = d.sum
  assume(num.gt(sum, num.zero), "At least one histogram element must be non-zero")
  /** Draws a random index according to histogram */
  def apply(rng: TRandom): Int = {
    val r = num.toDouble( sum) * rng.nextDouble
    var theSum: Double = 0
    d.indexWhere(e => { theSum += num.toDouble(e); theSum >= r })
  }
  /** Draws multiple indices *without replacement* */
 def apply(rng: TRandom, n: Int): Seq[Int] = {
    assume(n <= d.size)
    @tailrec def draw(k: Int, s: Double, remaining: Set[Int], selected: List[Int]): Seq[Int] = k match {
      case 0 => selected
      case _ => {
        val r = s * rng.nextDouble
        var theSum: Double = 0
        val rem = remaining.toSeq
        val iter = rem.iterator
        var last = -1
        do {
          theSum += num.toDouble(d(iter.next))
          last = last + 1
        } while (iter.hasNext && theSum < r)
        draw(k - 1, s - num.toDouble(d(last)), remaining.take(last) ++ iter.toSet, 
            rem(last) :: selected)
      }
    }
    draw(n,num.toDouble( sum), 0.until(d.size).toSet, List[Int]())
  }
}
object Histogram {
  def apply[T](d: Seq[T])(implicit num: Numeric[T]) = new Histogram(d)(num)
}

class Distribution(d: Seq[Double]) extends Histogram(d) {
  // This was problematic due to numerical roundoffs
  // assume(d.sum == 1.0, "Distribution should sum up to 1.0. And this one is: " + d)
  assume(sum > 0.999 && sum < 1.001, "Distribution should sum up to 1.0. And this one is: " + d)
}
object Distribution {
  def apply(d: Seq[Double]) = new Distribution(d)
  def fromAnything(d: Seq[Double]) = new Distribution(d.map(_ / d.sum))
}

