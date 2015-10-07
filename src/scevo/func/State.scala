package scevo.func

import scevo.evo.State


/** A working population is a state storing a set of solutions 
 *  
 */
trait StatePop[T] extends State {
  def solutions: Seq[T]
}

class Population[T](override val solutions: Seq[T], override val iteration: Int = 0)
    extends StatePop[T] {
  require(solutions.size > 0, "The set of solutions in a population cannot be empty")
  def this(s1: StatePop[T], s2: StatePop[T]) = this(s1.solutions ++ s2.solutions,
    math.max(s1.iteration, s2.iteration))
}

object Population {
  def apply[T](sols: Seq[T], iter: Int = 0) = new Population[T](sols, iter) 
}
