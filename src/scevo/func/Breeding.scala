package scevo.func

import scala.annotation.tailrec
import scala.collection.immutable.Stream.consWrapper
import scevo.moves.Moves
import scevo.core.Dominance
import scevo.util.Options
import scevo.util.TRandom
import scevo.core.StatePop
import scevo.core.Population

/**
  * Performs breeding, i.e., selection followed by application of search operators
  * (feasibility check is assumed to be done within SearchOperators).
  *
  * Breeder combines these two actions because the number of solutions returned
  * by search operators may vary (even between the calls of the same operator), among
  * others because some of the produced solutions may not pass faeasibility check.
  * Therefore, it
  * is in general impossible to determine in advance how many selection acts and
  * applications of search operators may be needed to populate next population.
  *
  */
class Breeder[S, E](val sel: Selection[S, E],
                    val searchOperator: () => SearchOperator[S]) {

  def selStream(src: Seq[(S, E)]): Stream[S] = sel(src)._1 #:: selStream(src)

  def breedn(n: Int, s: Seq[(S, E)]) = {

    @tailrec def breed(parStream: Stream[S], offspring: List[S] = List()): Seq[S] =
      if (offspring.size >= n)
        offspring.take(n)
      else {
        val (off, parentTail) = searchOperator()(parStream)
        breed(parentTail, offspring ++ off)
      }

    breed(selStream(s))
  }
}

/**
  * In generational breeding, all bred solutions are new, and none of them has
  *  an evaluation.
  *
  *  Note that in this setup, even if a new child solution is a clone of its parent,
  *  we lose its evaluation anyway.
  */
trait GenerationalBreeder[S, E] extends (StatePop[(S, E)] => StatePop[S])

class SimpleBreeder[S, E](override val sel: Selection[S, E],
                          override val searchOperator: () => SearchOperator[S])
    extends Breeder[S, E](sel, searchOperator) with GenerationalBreeder[S, E] {

  override def apply(s: StatePop[(S, E)]) = Population(breedn(s.size, s))
}
object SimpleBreeder {
  def apply[S, E](sel: Selection[S, E],
                  searchOperator: () => SearchOperator[S]) = new SimpleBreeder[S, E](sel, searchOperator)

}

/**
  * For steady-state EA, only one solution is added in each generation (and one
  *  removed). The other solutions remain intact, so the signature of the breeder
  *  must be different.
  */
trait SteadyStateBreeder[S, E] extends (StatePop[(S, E)] => StatePop[(S, E)])

class SimpleSteadyStateBreeder[S, E](override val sel: Selection[S, E],
                                     override val searchOperator: () => SearchOperator[S],
                                     val desel: Selection[S, E],
                                     val eval: S => E)
    extends Breeder[S, E](sel, searchOperator) with SteadyStateBreeder[S, E] {

  override def apply(s: StatePop[(S, E)]) = {
    val b = breedn(1, s).head
    val r = (b, eval(b))
    val toRemove = desel(s)
    val pos = s.indexOf(toRemove)
    val (h, t) = s.splitAt(pos)
    Population(h ++ Seq(r) ++ t.tail)
  }
}

/**
  * Breeding in NSGA is a bit tricky: it requires first ranking, then tournament
  *  selection.
  *
  *  Warning: this breeder does not merge parents and children
  */
class NSGABreeder[S, E](domain: Moves[S])(
  implicit opt: Options, rng: TRandom, ordering: Dominance[E])
    extends GenerationalBreeder[S, Seq[E]] {
  val nsga = new NSGA2Selection[S, E](opt)(rng)
  val breeder = SimpleBreeder[S, Rank[E]](nsga, RandomMultiOperator(domain.moves: _*))
  override def apply(s: StatePop[(S, Seq[E])]) = {
    val ranking = nsga.rank(s.size, ordering)(s)
    breeder(Population[(S, Rank[E])](ranking))
  }
}

/**
  * This breeder merges the previous population with parents in the mu+lambda style.
  *
  */
class NSGABreederElitist[S, E](domain: Moves[S])(
  implicit opt: Options, rng: TRandom, ordering: Dominance[E])
    extends GenerationalBreeder[S, Seq[E]] {
  val nsga = new NSGA2Selection[S, E](opt)(rng)
  val breeder = SimpleBreeder[S, Rank[E]](nsga, RandomMultiOperator(domain.moves: _*))
  var previous = Seq[(S, Seq[E])]()
  override def apply(s: StatePop[(S, Seq[E])]) = {
    val merged = s ++ previous
    val ranking = nsga.rank(s.size, ordering)(merged)
    previous = ranking.map(s => (s._1, s._2.eval))
    breeder(Population[(S, Rank[E])](ranking))
  }
}
 