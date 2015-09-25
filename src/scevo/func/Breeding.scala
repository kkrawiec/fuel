package scevo.func

import scevo.tools.Options
import scevo.tools.TRandom
import scala.annotation.tailrec
import scevo.Distribution

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
  * Pulling parents from the previous population is implemented with streams (could be alternatively
  * done with iterators, but iterators are mutable). A search operator fetches parents from the stream and
  * returns the offspring(s) (as a List), *and* the tail of the parent stream (typically;
  *
  */
/*
trait Breeder {
  def apply[S, E](
    sel: Seq[(S, E)] => (S, E),
    searchOperator: () => (Stream[S] => (List[S], Stream[S])),
    isFeasible: S => Boolean = (_: S) => true)
}
* 
*/

object SimpleBreeder {//}extends Breeder {
  def apply[S, E](
    sel: Seq[(S, E)] => (S, E),
    searchOperator: () => (Stream[S] => (List[S], Stream[S])),
    isFeasible: S => Boolean = (_: S) => true) = {

    def selStream(src: Seq[(S, E)]): Stream[S] = sel(src)._1 #:: selStream(src)

    current: StatePop[(S, E)] => {
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
}

// Picks one of the functions (pipes) at random to 
object RandomMultiBreeder {
  def apply[S](rng: TRandom, config: Options)(pipes: Seq[Stream[S] => (List[S], Stream[S])]) = {
    val prob = config.paramString("operatorProbs")
    val distribution = Distribution(
      if (prob.isDefined)
        prob.get.split(",").map(_.toDouble)
      else {
        println("Probability distribution for operators undefined. Equal probabilities set.")
        val p = List.fill(pipes.size - 1)(1.0 / pipes.size)
        (1.0 - p.sum) :: p
      })
    require(distribution.d.size == pipes.size, "Invalid number of operator probabilities")
    () => pipes(distribution(rng))
  }
}