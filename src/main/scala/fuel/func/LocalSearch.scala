package fuel.func

import fuel.moves.Neighborhood
import scala.annotation.tailrec
import fuel.util.Options
import scala.Stream

/**
  * Implements greedy local search, where State maintains one candidate solution.
  *
  */
class LocalSteepest[S, E](neighborhood: Neighborhood[S],
                          eval: S => E,
                          stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                            implicit opt: Options, ord: Ordering[E])
    extends IterativeSearch[S] {

  override def iter = (s: S) => (neighborhood(s).+:(s)).map(e => (e, eval(e))).minBy(_._2)._1
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
    extends IterativeSearch[(S,E)] {

  override def iter = (s: (S, E)) => {
    @tailrec def firstImproving(str: Stream[S]): Option[(S, E)] = str match {
      case Stream.Empty => None
      case h #:: tail => {
        val e = eval(h)
        if (ord.lt(e, s._2)) Some((h, e))
        else firstImproving(tail)
      }
    }
    val neigh = neighborhood(s._1)
    firstImproving(neigh).getOrElse(s)
  }

  def apply(s: S) = super.apply((s, eval(s)))
  override def terminate = Seq(Termination.MaxIter(it), Termination.MaxTime(opt))
}

