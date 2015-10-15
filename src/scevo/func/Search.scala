package scevo.func

import scevo.util.Options
import scevo.Distribution
import scevo.util.TRandom

/**
  * Search operator is any function that generates new solutions from existing solutions.
  * The SearchOperatror trait is intended to 'wrap' the actual move (a function Solution(s) => Solution(s))
  * and optionally combine it with feasibility check. 
  *
  * Pulling parents from the previous population is implemented with streams (could be alternatively
  * done with iterators, but iterators are mutable). A search operator fetches parents from the stream and
  * returns the offspring(s) (as a List), *and* the tail of the parent stream (typically; if required
  * the input stream may remain intact).
  *
  */

trait SearchOperator[S] extends (Stream[S] => (List[S], Stream[S]))

class SearchOperator1[S](body: S => S,
                         isFeasible: S => Boolean = (_: S) => true)
    extends SearchOperator[S] {
  override def apply(s: Stream[S]) = (List(body(s.head)).filter(isFeasible(_)), s.tail)
}

class SearchOperator2[S](body: Function2[S, S, (S, S)],
                         isFeasible: S => Boolean = (_: S) => true)
    extends SearchOperator[S] {
  override def apply(s: Stream[S]) = {
    val r = body(s(0), s(1))
    (List(r._1, r._2).filter(isFeasible(_)), s.drop(2))
  }
}

class SearchOperator2_1[S](body: Function2[S, S, S],
                           isFeasible: S => Boolean = (_: S) => true)
    extends SearchOperator[S] {
  override def apply(s: Stream[S]) = (List(body(s(0), s(1))).filter(isFeasible(_)), s.drop(2))
}

/* A bit verbose because of compiler's complaint "multiple overloaded alternatives of method apply define default arguments. "
 * 
 */
object SearchOperator {
  def apply[S](body: S => S) = new SearchOperator1[S](body)
   def apply[S](body: S => S, isFeasible: S => Boolean ) =
    new SearchOperator1[S](body, isFeasible)

  def apply[S](body: Function2[S, S, (S, S)]) = new SearchOperator2[S](body )
   def apply[S](body: Function2[S, S, (S, S)], isFeasible: S => Boolean ) =
    new SearchOperator2[S](body, isFeasible)

  def apply[S](body: Function2[S, S, S]) = new SearchOperator2_1[S](body )
  def apply[S](body: Function2[S, S, S], isFeasible: S => Boolean ) =
    new SearchOperator2_1[S](body, isFeasible)
}

// Picks one of the functions (pipes) at random to 
object RandomMultiOperator {
  def apply[S](pipes: SearchOperator[S]*)(implicit config: Options, rng: TRandom) = {
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

