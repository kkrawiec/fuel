package scevo.core

/**
  * Search state. Can be basically anything. 
  * 
  * When defining your own derived State class, consider mixing-in the Serializable trait. 
  */
trait State 

/**
  * A working population is a state storing a Seq of solutions
  *  (duplicates are thus allowed).
  *
  * The state itself implements the Seq trait, so it behaves like a sequence.
  */
trait StatePop[T] extends State with Seq[T]

class Population[T](val solutions: Seq[T]) extends StatePop[T] {
  assume(solutions.size > 0, "Population cannot be empty")
  def iterator = solutions.iterator
  def apply(idx: Int) = solutions(idx)
  def length = solutions.length
}

object StatePop {
  def apply[T](sols: Seq[T]) = new Population[T](sols)
}


/** Traits and classes for representing single-solution states,
 *  mostly for the local search algorithms. 
 */
trait StateSingle[T] extends State {
  def get: T
}

class StateOne[T](override val get: T)
  extends StateSingle[T]

object StateSingle {
  def apply[T](solution: T) = new StateOne(solution)
}