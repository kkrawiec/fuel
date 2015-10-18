package scevo.core


/** Search state. 
 */

trait State extends Serializable 

/** A working population is a state storing a Seq of solutions 
 *  (duplicates are thus allowed). 
 * 
 * The state itself implements the Seq trait, so it behaves like a sequence.   
 */
trait StatePop[T] extends State with Seq[T] {
  protected def solutions: Seq[T]
}

class Population[T](override val solutions: Seq[T])
    extends StatePop[T] {
  require(solutions.size > 0, "The set of solutions in a population cannot be empty")
//  def this(s1: StatePop[T], s2: StatePop[T]) = this(s1.solutions ++ s2.solutions)
  
  def iterator: Iterator[T] = solutions.iterator
  def apply(idx: Int): T = solutions(idx)
  def length: Int = solutions.length
}

/** The implicit parameter facilitates creating a population with the same iteration 
 *  number. 
 */
object Population {
  def apply[T](sols: Seq[T]) = new Population[T](sols) 
}
