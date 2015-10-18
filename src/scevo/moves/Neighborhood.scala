package scevo.moves

import scevo.func.Termination
import scevo.func.ParallelEval
import scevo.func.IterativeSearch
import scevo.util.Options
import scevo.core.StatePop
import scevo.func.RandomStatePop
import scevo.core.State

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

class LocalSearch[S, E](neighborhood: Neighborhood[S],
                        eval: S => E,
                        stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                          implicit opt: Options, ord: Ordering[E])
    extends IterativeSearch[StateSingle[(S, E)]] {

  override def iter: Function1[StateSingle[(S, E)], StateSingle[(S, E)]] = (s: StateSingle[(S, E)]) => {
    val best = neighborhood(s.solution._1).map(e => (e, eval(e))).minBy(_._2)
    StateOne(best, s.iteration + 1)
  }
  def apply(s: S) = super.apply(StateOne((s, eval(s))))
  override def terminate = Seq(Termination.MaxIter(opt), Termination.MaxTime(opt))
}

trait StateSingle[T] extends State {
  def solution: T
}

class StateOne[T](override val solution: T, override val iteration: Int = 0)
  extends StateSingle[T]

object StateOne {
  def apply[T](solution: T, iteration: Int = 0) = new StateOne(solution, iteration)
}