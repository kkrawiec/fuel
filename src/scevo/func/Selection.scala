package scevo.func

import scala.annotation.tailrec
import scevo.Distribution
import scevo.Preamble.RndApply
import scevo.util.Options
import scevo.util.TRandom
import scevo.core.Greatest

/**
  * Selection can be applied to any set (Seq, so duplicates are OK) of solutions,
  * not only populations, hence the signature.
  */
trait Selection[S, E] extends (Seq[(S, E)] => (S, E))

class GreedySelection[S, E](implicit o: Ordering[E]) extends Selection[S, E] {
  def apply(pop: Seq[(S, E)]) = pop.minBy(_._2)
}

abstract class StochasticSelection[S, E](val rand: TRandom) extends Selection[S, E]

class RandomSelection[S, E](implicit rand: TRandom) extends StochasticSelection[S, E](rand) {
  override def apply(pop: Seq[(S, E)]) = pop(rand)
}

class TournamentSelection[S, E](ordering: Ordering[E], val tournamentSize: Int)(implicit rng: TRandom)
    extends StochasticSelection[S, E](rng) {

  def this(o: Ordering[E])(implicit opt: Options, rng: TRandom) =
    this(o, opt('tournamentSize, 7, (_: Int) >= 2))(rng)

  def apply(pop: Seq[(S, E)]) = pop(rand, tournamentSize).minBy(_._2)(ordering)
}
object TournamentSelection {
  def apply[S, E](o: Ordering[E])(implicit opt: Options, rand: TRandom) =
    new TournamentSelection[S, E](o)(opt, rand)
  def apply[S, E](opt: Options)(rand: TRandom)(o: Ordering[E]) =
    new TournamentSelection[S, E](o)(opt, rand)
}


/** Partial tournament: the winner is the solution that dominates all the remaining in
 *  the pool, or if no such solution exists then a randomly picked solution. 
 *  
 *  Also known as dominance tournament. 
 */
class PartialTournament[S, E](val tournamentSize: Int)(implicit ordering: PartialOrdering[E], rng: TRandom)
    extends StochasticSelection[S, E](rng) {

  implicit val ord = new PartialOrdering[(S,E)]{
    override def tryCompare(a: (S,E), b:(S,E)) = ordering.tryCompare(a._2, b._2)
    override def lteq(a: (S,E), b:(S,E)) = ordering.lteq(a._2, b._2)
  }
//  def this(o: Ordering[E])(implicit opt: Options, rand: TRandom) =
//    this(opt('tournamentSize, 2, (_: Int) >= 2))(o, rand)

  def apply(pop: Seq[(S, E)]) = {
    val sample = pop(rand, tournamentSize)
    Greatest(sample).getOrElse(sample(rng))
  }
}


/**
  * Fitness-proportionate selection: draws a solution proportionally to its fitness.
  *
  *  This is a rather inefficient implementation, as it recalculates the distribution of
  *  fitness for entire population in every
  *  act of selection. This could be sped up by first normalizing the fitness in the entire
  *  population and then drawing a number from [0,1]. However, fitness-proportionate selection
  *  has its issues and is not in particularly wide use today, so this simple implementation
  *  should be sufficient.
  */
class FitnessPropSelSlow[S](implicit rand: TRandom) extends Selection[S, Double] {
  def apply(pop: Seq[(S, Double)]) = {
    val distribution = Distribution.fromAnything(pop.map(_._2))
    pop(distribution(rand))
  }
}

/**
  * Lexicase selection by Spector et al. Applicable to test-based problems.
  *
  * Iteratively selects a random test and keeps only the solutions that pass it.
  * It does so until only one solution is left, and that solution is the outcome of selection.
  *
  * Note: Here E stands for one objective, not entire evaluation.
  */
class LexicaseSelection[S, E](o: Ordering[E])(implicit rand: TRandom)
    extends StochasticSelection[S, Seq[E]](rand) {
  def apply(pop: Seq[(S, Seq[E])]) = {
    @tailrec def sel(sols: Seq[(S, Seq[E])], cases: List[Int]): (S, Seq[E]) =
      if (sols.size == 1) sols(0)
      else if (cases.size == 1) sols(rand)
      else {
        val theCase = cases(rand)
        val ord = new Ordering[(S, Seq[E])] {
          override def compare(a: (S, Seq[E]), b: (S, Seq[E])) = o.compare(a._2(theCase), b._2(theCase))
        }
        val best = sols.min(ord)
        //println("Sols:" + sols.size + " Cases: " + cases.size)
        sel(sols.filter(s => ord.compare(s, best) <= 0), cases.diff(List(theCase)))
      }
    // assumes nonempty pop
    sel(pop, 0.until(pop(0)._2.size).toList)
  }
}
