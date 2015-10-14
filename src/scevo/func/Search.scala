package scevo.func

import scevo.util.Options
import scevo.Distribution
import scevo.util.TRandom

/**
  * Search operator is any function that generates new solutions from existing solutions.
  *
  * Pulling parents from the previous population is implemented with streams (could be alternatively
  * done with iterators, but iterators are mutable). A search operator fetches parents from the stream and
  * returns the offspring(s) (as a List), *and* the tail of the parent stream (typically; if required
  * the input stream may remain intact). 
  *
  */

trait SearchOperator[S] extends (Stream[S] => (List[S], Stream[S]))

class SearchOperator1[S](body: S => S) extends SearchOperator[S] {
  override def apply(s: Stream[S]) = (List(body(s.head)), s.tail)
}

class SearchOperator2[S](body: Function2[S, S, (S, S)]) extends SearchOperator[S] {
  override def apply(s: Stream[S]) = {
    val r = body(s(0), s(1))
    (List(r._1, r._2), s.drop(2))
  }
}

class SearchOperator2_1[S](body: Function2[S, S, S]) extends SearchOperator[S] {
  override def apply(s: Stream[S]) = (List(body(s(0), s(1))), s.drop(2))
}

object SearchOperator {
  def apply[S](body: S => S) = new SearchOperator1[S](body)
  def apply[S](body: Function2[S, S, (S, S)]) = new SearchOperator2[S](body)
  def apply[S](body: Function2[S, S, S]) = new SearchOperator2_1[S](body)
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

