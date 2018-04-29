package fuel.moves

import fuel.func.SearchOperator

/** A helper trait that defines moves for a given space of solutions of type S. 
 *  
 *  Intended to be used as a *set* of moves to be used by a search algorithm. 
 *  
 *  For brevity, we are assuming that a function generating a new solution is 
 *  also a move (indeed it is, a move from nowhere to S: Unit => S).  
 *  
 */
trait Moves[S] extends Seq[SearchOperator[S]]{
  def newSolution: S
  def moves: Seq[SearchOperator[S]]

  def iterator = moves.iterator
  def apply(idx: Int) = moves(idx)
  def length = moves.length
}