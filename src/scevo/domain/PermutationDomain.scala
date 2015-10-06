package scevo.domain

import scala.Range

import scevo.func.SearchOperator1
import scevo.tools.TRandom

/**
  *  Simple domain of permutations. Candidate solutions are permutations of n elements.
  *
  */
class PermutationDomain(n: Int)(rng: TRandom)
    extends Domain[Seq[Int]] with Moves[Seq[Int]] {
  require(n > 0)

  override def randomSolution = rng.shuffle(Range(0, n).toIndexedSeq)

  def mutation = SearchOperator1((p: Seq[Int]) => {
    val (c1, c2) = (rng.nextInt(n), rng.nextInt(n))
    val h = p(c1)
    p.updated(c1, p(c2)).updated(c2, h)
  })
  override def moves = Seq(mutation)
}

object PermutationDomain {
  def apply(n: Int)(implicit rng: TRandom) = new PermutationDomain(n)(rng)
}

