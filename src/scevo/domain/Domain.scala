package scevo.domain

import scevo.func.SearchOperator

trait Domain[S] {
  def randomSolution: S
}

trait Moves[S] {
  def moves: Seq[SearchOperator[S]]
}