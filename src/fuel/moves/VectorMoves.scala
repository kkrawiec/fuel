package fuel.moves

import fuel.util.TRandom

import fuel.moves.AbstractVectorMoves
abstract class VectorMoves[T](val numVars: Int)(rng: TRandom)
    extends AbstractVectorMoves[IndexedSeq[T]] {
  assert(numVars > 0)

  override def onePointCrossover = (p1: IndexedSeq[T], p2: IndexedSeq[T]) => {
    val cuttingPoint = rng.nextInt(numVars)
    val (myHead, myTail) = p1.splitAt(cuttingPoint)
    val (hisHead, hisTail) = p2.splitAt(cuttingPoint)
    (myHead ++ hisTail, hisHead ++ myTail)
  }

  override def twoPointCrossover = (p1: IndexedSeq[T], p2: IndexedSeq[T]) => {
    val h = (rng.nextInt(numVars), rng.nextInt(numVars))
    val c = if (h._1 <= h._2) h else h.swap
    val (myHead, myRest) = p1.splitAt(c._1)
    val (myMid, myTail) = myRest.splitAt(c._2)
    val (hisHead, hisRest) = p2.splitAt(c._1)
    val (hisMid, hisTail) = myRest.splitAt(c._2)
    (myHead ++ hisMid ++ myTail, hisHead ++ myMid ++ hisTail)
  }
}

