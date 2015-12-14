package fuel.example

import scala.Ordering
import scala.Range
import fuel.func.RunExperiment
import fuel.func.SimpleEA
import fuel.moves.BoolVectorMoves
import fuel.util.GetOptCollRng
import fuel.util.TRandom

/**
  * Use case: Hierarchical If and only If problem by Watson, Hornby and Pollack.  
  * 
  * See: http://www.cs.brandeis.edu/~richardw/hiff.html
  *
  * Maximized fitness function
  *
  */
object Hiff {
  def main(args: Array[String]) {
    implicit val (opt, coll, rng) = GetOptCollRng("--n 32 --trivial false --maxGenerations 1000")
    val n = opt('n, (_: Int) > 0)

    val prob = new HiffProblem(n, opt('trivial, false))
    val ga = SimpleEA(BoolVectorMoves(n), prob.hiff _, prob.maxEval)(opt, coll, rng,
      ordering = Ordering[Int].reverse)

    RunExperiment(ga)
  }
}

class HiffProblem(val n: Int, trivial: Boolean)(implicit rng: TRandom) {
  assert((n & (n - 1)) == 0, "The number of variables must be a power of 2.")
  val blocks =
    if (trivial) Range(0, n).toVector
    else rng.shuffle(Range(0, n).toVector)
  println(s"Block definition: $blocks")

  private def eval(s: Seq[Boolean]): Int = s.size match {
    case 1 => 1
    case _ => {
      val h = s.splitAt(s.size / 2)
      eval(h._1) + eval(h._2) + (if (s.toSet.size == 1) s.size else 0)
    }
  }
  def hiff(s: Seq[Boolean]) = eval(blocks.map(i => s(i)))

  private val log2n = (n - 1).toBinaryString.count(_ == '1')
  val maxEval = n * (log2n + 1)
}

