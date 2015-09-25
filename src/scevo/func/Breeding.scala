package scevo.func

import scala.annotation.tailrec
import scala.collection.immutable.Stream.consWrapper

/**
  * Performs breeding, i.e., selection followed by application of search operators
  *  followed by feasibility check.
  *
  * Breeder combines these three actions because (i) the number of solutions returned
  * by search operators may vary (even between calls of the same operator), and
  * (ii) some of the produced solutions may turn out to be infeasible. Therefore, it
  * is in general impossible to determine in advance how many selection acts and
  * applications of search operators may be needed to populate next population.
  *
  */

trait Breeder[S, E] extends (StatePop[(S, E)] => StatePop[S])

class SimpleBreeder[S, E](val sel: Selection[S, E],
                          val searchOperator: () => SearchOperator[S],
                          val isFeasible: S => Boolean = (_: S) => true)
    extends Breeder[S, E] {

  def selStream(src: Seq[(S, E)]): Stream[S] = sel(src)._1 #:: selStream(src)

  override def apply(current: StatePop[(S, E)]) = {
    @tailrec def breed(offspring: List[S], parStream: Stream[S]): Seq[S] =
      if (offspring.size >= current.solutions.size)
        offspring.take(current.solutions.size)
      else {
        val (off, parentTail) = searchOperator()(parStream)
        breed(offspring ++ off.filter(isFeasible), parentTail)
      }
    val parentStream = selStream(current.solutions)
    Population(breed(List[S](), parentStream), current.iteration + 1)
  }
}

object SimpleBreeder {
  def apply[S, E](sel: Selection[S, E],
                  searchOperator: () => SearchOperator[S],
                  isFeasible: S => Boolean = (_: S) => true) =
    new SimpleBreeder(sel, searchOperator, isFeasible)
}
 