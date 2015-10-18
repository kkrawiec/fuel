package scevo.moves

import scala.IndexedSeq
import scala.Stream
import scala.collection.immutable.Stream.consWrapper

/**
  * Neigborhood is a Stream of solutions, so that it can be infinite.
  *
  * Note: neighborhood does not contain the reference (original) solution.
  */
trait Neighborhood[S] extends Function1[S, Stream[S]]

class BoolVectNeigh extends Neighborhood[IndexedSeq[Boolean]] {

  def apply(s: IndexedSeq[Boolean]) = {
    val allNeigh = for (i <- 0 until s.size) yield s.updated(i, !s(i))
    def stream(i: Int): Stream[IndexedSeq[Boolean]] = i match {
      case -1 => Stream.Empty
      case _  => allNeigh(i) #:: stream(i - 1)
    }
    stream(s.size - 1)
  }
}

object TestNeigh extends App {
  println((new BoolVectNeigh)(IndexedSeq(false, false, false)))
  println((new BoolVectNeigh)(IndexedSeq(false, false, false)).toList)
}
