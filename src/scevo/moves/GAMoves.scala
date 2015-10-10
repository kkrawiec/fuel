package scevo.moves

import scevo.func.SearchOperator1
import scevo.func.SearchOperator2

/**
  * The default set of operators for vector representations
  */

trait GAMoves[S] extends Moves[S] {
  def oneBitMutation: Function1[S,S]
  def onePointCrossover: Function2[S, S, (S, S)]
  def twoPointCrossover: Function2[S, S, (S, S)]
  override def moves = Seq(
    SearchOperator1(oneBitMutation),
    SearchOperator2(onePointCrossover),
    SearchOperator2(twoPointCrossover))
}

