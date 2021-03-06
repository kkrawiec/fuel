package fuel.example

import scala.Ordering
import fuel.func.SearchOperator
import fuel.func.SimpleEA
import fuel.moves.BoolVectorMoves
import fuel.util.GetOptCollRng
import fuel.func.RunExperiment

/**
  * Use case: MaxOnes with Vectors.
  *
  * Maximized fitness function.
  *
  * The parameters passed to main() override the ones specified in the Array.
  *
  */
object MaxOnesVectors extends App {
  // Allow the command-line arguments overwrite the default ones:
  implicit val (opt, coll, rng) = GetOptCollRng(Array("--numVars", "200", "--maxGenerations", "200")
    ++ args)

  // Say we want a different setup of search operators than the default one:
  // just one operator doing 2-bit mutation. 
  //  We can achieve that by composing two one-bit mutations:
  object MyMoves extends BoolVectorMoves(opt('numVars, (_: Int) > 0)) {
    override def moves = Seq(SearchOperator(onePointMutation compose onePointMutation))
  }

  // Want to maximize the objective:
  implicit val ord = Ordering[Int].reverse

  val ga = SimpleEA(moves = MyMoves,
    eval = (s: IndexedSeq[Boolean]) => s.count(_ == true),
    stop = (s: IndexedSeq[Boolean], e: Int) => e == MyMoves.numVars)

  RunExperiment(ga)
}
