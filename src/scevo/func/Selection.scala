package scevo.func

import scala.annotation.tailrec

import scevo.Distribution
import scevo.Preamble.RndApply
import scevo.util.Options
import scevo.util.TRandom

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

class TournamentSelection[S, E](ordering: Ordering[E], val tournamentSize: Int)(implicit rand: TRandom)
    extends StochasticSelection[S, E](rand) {

  def this(o: Ordering[E])(implicit opt: Options, rand: TRandom) =
    this(o, opt("tournamentSize", 7, (_: Int) >= 2))(rand)

  def apply(pop: Seq[(S, E)]) = pop(rand, tournamentSize).minBy(_._2)(ordering)
}
object TournamentSelection {
  def apply[S, E](o: Ordering[E])(implicit opt: Options, rand: TRandom) =
    new TournamentSelection[S, E](o)(opt, rand)
  def apply[S, E](opt: Options)(rand: TRandom)(o: Ordering[E]) =
    new TournamentSelection[S, E](o)(opt, rand)
}

/**
  * Fitness-proportionate selection: draws a solution proportionally to its fitness.
  *
  *  This is a rather inefficient implementation, as it recalculates the distribution in every
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
