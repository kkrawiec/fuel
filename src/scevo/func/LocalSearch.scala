package scevo.func

import scevo.core.StateSingle
import scevo.moves.Neighborhood
import scevo.util.CallCounter
import scevo.util.Options

class LocalSearch[S, E](neighborhood: Neighborhood[S],
                        eval: S => E,
                        stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                          implicit opt: Options, ord: Ordering[E])
    extends IterativeSearch[StateSingle[(S, E)]] {

  override protected val it = CallCounter(
    (s: StateSingle[(S, E)]) =>
      StateSingle(neighborhood(s.get._1).map(e => (e, eval(e))).minBy(_._2)))
  override def iter: Function1[StateSingle[(S, E)], StateSingle[(S, E)]] = it
  def apply(s: S) = super.apply(StateSingle((s, eval(s))))
  override def terminate = Seq(Termination.MaxIter(it), Termination.MaxTime(opt))
}

