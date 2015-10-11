package scevo.moves

import scevo.func.SearchOperator


/**
  * The default set of operators for vector representations
  */

trait AbstractVectorMoves[S] extends Moves[S] {
  def onePointMutation: Function1[S,S]
  def onePointCrossover: Function2[S, S, (S, S)]
  def twoPointCrossover: Function2[S, S, (S, S)]
  override def moves = Seq(
    SearchOperator(onePointMutation),
    SearchOperator(onePointCrossover),
    SearchOperator(twoPointCrossover))
}

