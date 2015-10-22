package scevo.moves

import scala.Range
import scevo.util.TRandom
import scevo.func.SearchOperator

/**
  *  Simple domain of permutations. Candidate solutions are permutations of n elements.
  *
  */
abstract class AbstractPermutationMoves[T](n: Int)(rng: TRandom)
    extends Moves[Seq[T]] {
  assume(n > 0)

  // Naive mutation: just swap two cities. 
  def mutation = (p: Seq[T]) => {
    val (c1, c2) = (rng.nextInt(n), rng.nextInt(n))
    val h = p(c1)
    p.updated(c1, p(c2)).updated(c2, h)
  }

  // Arc-swapping mutation
  def mutationArcSwapping = (p: Seq[T]) => {
    val t = (rng.nextInt(n), rng.nextInt(n))
    val (s, e) = if(t._1 < t._2) t else t.swap
    p.take(s) ++ p.slice(s, e).reverse ++ p.drop(e) 
  }

  override def moves = Seq(SearchOperator(mutationArcSwapping))
}

class PermutationMoves(n: Int)(rng: TRandom)
    extends AbstractPermutationMoves[Int](n)(rng) {

  override def newSolution = rng.shuffle(Range(0, n).toIndexedSeq)
}

object PermutationMoves {
  def apply(n: Int)(implicit rng: TRandom) = new PermutationMoves(n)(rng)
}

