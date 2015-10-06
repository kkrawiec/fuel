package scevo.domain

import scevo.func.SearchOperator1
import scevo.func.SearchOperator2

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

