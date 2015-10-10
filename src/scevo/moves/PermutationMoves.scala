package scevo.moves

import scala.Range
import scevo.tools.TRandom
import scevo.func.SearchOperator

/**
  *  Simple domain of permutations. Candidate solutions are permutations of n elements.
  *
  */
class PermutationMoves(n: Int)(rng: TRandom)
    extends Moves[Seq[Int]] {
  require(n > 0)

  override def newSolution = rng.shuffle(Range(0, n).toIndexedSeq)

  def mutation = SearchOperator((p: Seq[Int]) => {
    val (c1, c2) = (rng.nextInt(n), rng.nextInt(n))
    val h = p(c1)
    p.updated(c1, p(c2)).updated(c2, h)
  })
  override def moves = Seq(mutation)
}

object PermutationMoves {
  def apply(n: Int)(implicit rng: TRandom) = new PermutationMoves(n)(rng)
}

