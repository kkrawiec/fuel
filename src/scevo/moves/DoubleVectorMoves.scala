package scevo.moves

import scevo.util.TRandom

class DoubleVectorMoves(numVars: Int, sigma: Double)(implicit rng: TRandom)
    extends VectorMoves[Double](numVars)(rng) {

  override def newSolution = IndexedSeq.fill(numVars)(rng.nextDouble)

  override def onePointMutation = (p: IndexedSeq[Double]) => {
    val xiToMutate = rng.nextInt(numVars)
    p.updated(xiToMutate, p(xiToMutate) + sigma * (rng.nextDouble - 0.5))
  }
}