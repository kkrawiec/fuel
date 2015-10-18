package scevo.core

/** Search state. 
 */

trait State extends Serializable {
  def iteration: Int
}

/** A working population is a state storing a Seq of solutions 
 *  (duplicates are thus allowed). 
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

/** The implicit parameter facilitates creating a population with the same iteration 
 *  number. 
 */
object Population {
  def apply[T](sols: Seq[T], iter: Int = 0) = new Population[T](sols, iter) 
  def apply[T](sols: Seq[T])(implicit s: StatePop[T]) = new Population[T](sols, s.iteration) 
}
