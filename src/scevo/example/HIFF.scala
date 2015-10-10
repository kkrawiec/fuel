package scevo.example

import scevo.func.Experiment
import scevo.func.SimpleEA
import scevo.tools.OptCollRng
import scevo.moves.BoolVectorMoves
import scevo.Preamble._
import scala.Ordering
import scala.Range

/**
  * Use case: Hierarchical If and only If
  *
  * Maximized fitness function
  */
object Hiff {
  def main(args: Array[String]) {
    implicit val (opt, coll, rng) = OptCollRng("--n 32 --trivial false --maxGenerations 1000")
    val n = opt.paramInt("n", _ > 0)
    require((n & (n - 1)) == 0, "The number of variables must be a power of 2.")

    val blocks =
      if (opt.paramBool("trivial")) Range(0, n).toVector
      else rng.shuffle(Range(0, n).toVector)
    println(s"Block definition: $blocks")

    def eval(s: Seq[Boolean]): Int = s.size match {
      case 1 => 1
      case _ => {
        val h = s.splitAt(s.size / 2)
        eval(h._1) + eval(h._2) + (if (s.toSet.size == 1) s.size else 0)
      }
    }
    def hiff(s: Seq[Boolean]) = eval(blocks.map(i => s(i)))

    val log2n = (n - 1).toBinaryString.count(_ == '1')
    val maxEval = n * (log2n + 1)
    val ga = SimpleEA(moves = BoolVectorMoves(n),
      eval = eval,
      stop = (s: Seq[Boolean], e: Int) => e == maxEval)(opt, coll, rng,
        ordering = Ordering[Int].reverse)

    Experiment.run(ga)
  }
}
