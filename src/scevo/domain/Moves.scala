package scevo.domain

import scevo.func.SearchOperator

/** A helper trait that defines moves for a given space of solutions of type S. 
 *  
 *  For brevity, we are assuming that a function generating a new solution is 
 *  also a move (indeed it is, a move from nowhere to S: Unit => S).  
 *  
 */
trait Moves[S] {
  def newSolution: S
  def moves: Seq[SearchOperator[S]]
}