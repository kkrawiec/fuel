package scevo.core

/**
  * A working population is a state storing a Seq of solutions
  *  (duplicates are thus allowed).
  *
  * The state itself implements the Seq trait, so it behaves like a sequence.
  * 
  * When defining your own derived State class, consider mixing-in the Serializable trait. 
  */
trait StatePop[+T] extends Seq[T]

class Population[T](val solutions: Seq[T]) extends StatePop[T] {
  assert(solutions.size > 0, "Population cannot be empty")
  def iterator = solutions.iterator
  def apply(idx: Int) = solutions(idx)
  def length = solutions.length
}

object StatePop {
  def apply[T](sols: Seq[T]) = new Population[T](sols)
}

