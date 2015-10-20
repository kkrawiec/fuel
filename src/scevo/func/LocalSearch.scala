package scevo.func

import scevo.core.StateSingle
import scevo.moves.Neighborhood
import scevo.util.CallCounter
import scevo.util.Options
import scala.annotation.tailrec

/**
  * Implements greedy local search, where State maintains one candidate solution.
  *
  */
class LocalSteepest[S, E](neighborhood: Neighborhood[S],
                          eval: S => E,
                          stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                            implicit opt: Options, ord: Ordering[E])
    extends IterativeSearch[StateSingle[(S, E)]] {

  override def iter = (s: StateSingle[(S, E)]) =>
    StateSingle(neighborhood(s.get._1).map(e => (e, eval(e))).minBy(_._2))
  def apply(s: S) = super.apply(StateSingle((s, eval(s))))
  override def terminate = Seq(Termination.MaxIter(it), Termination.MaxTime(opt))
}

/**
  * Implements hill climber: moves to the first improving neighbor. 
  * By doing so, it does not always need to evaluate the entire neighborhood, 
  * so it's particularly beneficial for large neighborhoods.  
  * 
  * Will get stuck if no improvement found. 
  *
  */
class LocalHillClimber[S, E](neighborhood: Neighborhood[S],
                             eval: S => E,
                             stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                               implicit opt: Options, ord: Ordering[E])
    extends IterativeSearch[StateSingle[(S, E)]] {

  override def iter = (s: StateSingle[(S, E)]) => {
    @tailrec def firstImproving(str: Stream[S]): Option[(S, E)] = str match {
      case Stream.Empty => None
      case h #:: tail => {
        val e = eval(h)
        if (ord.lt(e, s.get._2)) Some((h, e))
        else firstImproving(tail)
      }
    }
    val neigh = neighborhood(s.get._1)
    StateSingle(firstImproving(neigh).getOrElse(s.get))
  }

  def apply(s: S) = super.apply(StateSingle((s, eval(s))))
  override def terminate = Seq(Termination.MaxIter(it), Termination.MaxTime(opt))
}

