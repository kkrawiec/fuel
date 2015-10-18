package scevo.core

/** Search state. 
 */

trait State extends Serializable 

/** A working population is a state storing a Seq of solutions 
 *  (duplicates are thus allowed). 
 *  
 */
trait StatePop[T] extends State {
  def solutions: Seq[T]
}

class Population[T](override val solutions: Seq[T])
    extends StatePop[T] {
  require(solutions.size > 0, "The set of solutions in a population cannot be empty")
  def this(s1: StatePop[T], s2: StatePop[T]) = this(s1.solutions ++ s2.solutions)
}

/** The implicit parameter facilitates creating a population with the same iteration 
 *  number. 
 */
object Population {
  def apply[T](sols: Seq[T]) = new Population[T](sols) 
}
