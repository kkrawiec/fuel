package scevo.func

import scevo.util.Options
import scevo.util.CallCounter
import scevo.core.State
import scevo.moves.Neighborhood

class LocalSearch[S, E](neighborhood: Neighborhood[S],
                        eval: S => E,
                        stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                          implicit opt: Options, ord: Ordering[E])
    extends IterativeSearch[StateSingle[(S, E)]] {

  private val it = CallCounter(
    (s: StateSingle[(S, E)]) =>
      StateOne(neighborhood(s.solution._1).map(e => (e, eval(e))).minBy(_._2)))
  override def iter: Function1[StateSingle[(S, E)], StateSingle[(S, E)]] = it
  def apply(s: S) = super.apply(StateOne((s, eval(s))))
  override def terminate = Seq(Termination.MaxIter(it), Termination.MaxTime(opt))
}

trait StateSingle[T] extends State {
  def solution: T
}

class StateOne[T](override val solution: T)
  extends StateSingle[T]

object StateOne {
  def apply[T](solution: T) = new StateOne(solution)
}