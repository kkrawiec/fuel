package scevo.example

import scala.Ordering
import scevo.func.Experiment
import scevo.func.SearchOperator
import scevo.func.SimpleEA
import scevo.moves.BoolVectorMoves
import scevo.tools.OptCollRng
import scevo.func.RunExperiment

/**
  * Use case: MaxOnes with Vectors.
  *
  * Maximized fitness function.
  *
  *
  */
object MaxOnesVectors {
  def main(args: Array[String]) {
    // Allow the command-line arguments overwrite the default ones:
    implicit val (opt, coll, rng) = OptCollRng(Array("--numVars", "20", "--maxGenerations", "200")
      ++ args)

    // Say we want a different setup of search operators than the default one:
    // just one operator doing 2-bit mutation. 
    //  We can achieve that by composing two one-bit mutations:
    object MyMoves extends BoolVectorMoves(opt.paramInt("numVars", _ > 0)) {
      override def moves = Seq(SearchOperator(onePointMutation compose onePointMutation))
    }

    // Want to maximize the objective:
    implicit val ord = Ordering[Int].reverse

    val ga = SimpleEA(moves = MyMoves,
      eval = (s: IndexedSeq[Boolean]) => s.count(_ == true),
      stop = (s: IndexedSeq[Boolean], e: Int) => e == MyMoves.numVars)

    RunExperiment(ga)
  }
}
