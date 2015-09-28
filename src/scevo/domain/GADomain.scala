package scevo.domain

import scala.collection.immutable.BitSet

import scevo.func.SearchOperator1
import scevo.func.SearchOperator2
import scevo.tools.TRandom

/**
  * Bitstring domain implemented as BitSets
  * solutions represented as BitSets (TreeSet much slower)
  *
  */



trait GADomain[S] extends Domain[S] with Moves[S] {
  def oneBitMutation: SearchOperator1[S]
  def onePointCrossover: SearchOperator2[S]
  def twoPointCrossover: SearchOperator2[S]
  override def moves = Seq(oneBitMutation, onePointCrossover, twoPointCrossover)
}

class BitSetDomain(numVars: Int)(rng: TRandom)
    extends GADomain[BitSet] {
  require(numVars > 0)

  override def randomSolution = BitSet.empty ++
    (for (i <- 0.until(numVars); if (rng.nextBoolean)) yield i)

  override def oneBitMutation = SearchOperator1((p: BitSet) => {
    val bitToMutate = rng.nextInt(numVars)
    if (p(bitToMutate)) p - bitToMutate else p + bitToMutate
  })

  override def onePointCrossover = SearchOperator2((p1: BitSet, p2: BitSet) => {
    val cuttingPoint = rng.nextInt(numVars)
    val (myHead, myTail) = p1.splitAt(cuttingPoint)
    val (hisHead, hisTail) = p2.splitAt(cuttingPoint)
    (myHead ++ hisTail, hisHead ++ myTail)
  })

  override def twoPointCrossover = SearchOperator2((p1: BitSet, p2: BitSet) => {
    val h = (rng.nextInt(numVars), rng.nextInt(numVars))
    val c = if (h._1 <= h._2) h else h.swap
    val (myHead, myRest) = p1.splitAt(c._1)
    val (myMid, myTail) = myRest.splitAt(c._2)
    val (hisHead, hisRest) = p2.splitAt(c._1)
    val (hisMid, hisTail) = myRest.splitAt(c._2)
    (myHead ++ hisMid ++ myTail, hisHead ++ myMid ++ hisTail)
  })
}
object BitSetDomain {
  def apply(numVars: Int)(implicit rng: TRandom) = new BitSetDomain(numVars)(rng)
}


