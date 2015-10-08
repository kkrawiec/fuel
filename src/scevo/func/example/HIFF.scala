package scevo.func.example

import scala.collection.immutable.BitSet
import scevo.func.Experiment
import scevo.func.SimpleEA
import scevo.moves.BitSetMoves
import scevo.tools.OptCollRng
import scevo.moves.BoolVectorMoves
import scevo.Preamble._

/**
  * Use case: Hierarchical If and only If
  *
  * Minimized fitness function
  */
object Hiff {
  def main(args: Array[String]) {
    implicit val (opt, coll, rng) = OptCollRng("--numVars 32 --maxGenerations 1000")
    val numVars = opt.paramInt("numVars", _ > 0)
    require((numVars & (numVars - 1)) == 0, "The number of variables must be a power of 2.")

    def eval(s: Seq[Boolean]): Int = s.size match {
      case 2 => s(0) != s(1)
      case _ => {
        val h = s.splitAt(s.size / 2)
        eval(h._1) + eval(h._2) + s.size * h._1.zip(h._2).filter(p => p._1 == p._2).size
      }
    }
    val ga = new SimpleEA(moves = BoolVectorMoves(numVars),
      eval = eval,
      stop = (s: Seq[Boolean], e: Int) => e == 0)

    Experiment.run(ga)
  }
}
