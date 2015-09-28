package scevo.domain

import scevo.func.SearchOperator1
import scevo.tools.TRandom

/**
  * Bitstring domain implemented as vectors of Booleans.
  *
  * The implementations of crossovers are identical as in BitSetDomain, but pulling them up to GADomain
  * would be a bit tricky.
  */

class BoolVectorDomain(numVars: Int)(rng: TRandom)
    extends VectorDomain[Boolean](numVars)(rng) {

  override def randomSolution = IndexedSeq.fill(numVars)(rng.nextBoolean)

  override def oneBitMutation = SearchOperator1((p: IndexedSeq[Boolean]) => {
    val bitToMutate = rng.nextInt(numVars)
    p.updated(bitToMutate, !p(bitToMutate))
  })
}

object BoolVectorDomain {
  def apply(numVars: Int)(implicit rng: TRandom) = new BoolVectorDomain(numVars)(rng)
}
