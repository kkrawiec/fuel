package scevo.func

import scala.annotation.tailrec
import scala.collection.immutable.Stream.consWrapper
import scevo.tools.Options
import scevo.tools.TRandom
import scevo.domain.Moves
import scevo.domain.Domain
import scevo.evo.Dominance

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
  */
trait Breeder[S, E] extends (StatePop[(S, E)] => StatePop[S])

class SimpleBreeder[S, E](val sel: Selection[S, E],
                          val searchOperator: () => SearchOperator[S],
                          val isFeasible: S => Boolean = (_: S) => true)
    extends Breeder[S, E] {

  def selStream(src: Seq[(S, E)]): Stream[S] = sel(src)._1 #:: selStream(src)

  override def apply(current: StatePop[(S, E)]) = {
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

object SimpleBreeder {
  def apply[S, E](sel: Selection[S, E],
                  searchOperator: () => SearchOperator[S],
                  isFeasible: S => Boolean = (_: S) => true) =
    new SimpleBreeder(sel, searchOperator, isFeasible)
}

// Can't use implicit domain due to Scala compiler bug. 
/**
  * Breeding in NSGA is a bit tricky: it requires first ranking, then tournament
  *  selection.
  *
  *  Warning: this breeder does not merge parents and children
  */
class NSGABreeder[S, E](domain: Domain[S] with Moves[S])(
  implicit opt: Options, rng: TRandom, ordering: Dominance[E])
    extends Breeder[S, Seq[E]] {
  val nsga = new NSGA2Selection[S, E](opt)(rng)
  val breeder = SimpleBreeder[S, Rank[E]](nsga, RandomMultiOperator(domain.moves: _*))
  override def apply(s: StatePop[(S, Seq[E])]) = {
    val ranking = nsga.rank(s.solutions.size, ordering)(s.solutions)
    breeder(Population[(S, Rank[E])](ranking, s.iteration))
  }
}

/** This breeder merges the previous population with parents in the mu+lambda style. 
 *  
 */
class NSGABreederElitist[S, E](domain: Domain[S] with Moves[S])(
  implicit opt: Options, rng: TRandom, ordering: Dominance[E])
    extends Breeder[S, Seq[E]] {
  val nsga = new NSGA2Selection[S, E](opt)(rng)
  val breeder = SimpleBreeder[S, Rank[E]](nsga, RandomMultiOperator(domain.moves: _*))
  var previous = Seq[(S, Seq[E])]()
  override def apply(s: StatePop[(S, Seq[E])]) = {
    val merged = s.solutions ++ previous
    val ranking = nsga.rank(s.solutions.size, ordering)(merged)
    previous = ranking.map(s => (s._1, s._2.eval))
    breeder(Population[(S, Rank[E])](ranking, s.iteration))
  }
}
 