package scevo.func

import scevo.Distribution
import scevo.Preamble.RndApply
import scevo.evo.BestSelector
import scevo.tools.Options
import scevo.tools.TRandom

trait Selection[S, E] extends (Seq[(S, E)] => (S, E))

class GreedyBestSelection[S, E](o: Ordering[E]) extends Selection[S, E] {
  def apply(pop: Seq[(S, E)]) = BestSelector(pop, o)
}

abstract class StochasticSelection[S, E](val rand: TRandom) extends Selection[S, E]

class RandomSelection[S, E](implicit rand: TRandom) extends StochasticSelection[S, E](rand) {
  override def apply(pop: Seq[(S, E)]) = pop(rand)
}

class TournamentSelection[S, E](o: Ordering[E])(implicit opt: Options,rand: TRandom)
    extends StochasticSelection[S, E](rand) {
  val tournamentSize = opt.paramInt("tournamentSize", 7, _ >= 2)
  def apply(pop: Seq[(S, E)]) = BestSelector(pop(rand, tournamentSize), o)
}
object TournamentSelection {
  def apply[S, E](o: Ordering[E])(implicit opt: Options,rand: TRandom) =
    new TournamentSelection[S, E](o)(opt, rand)
  def apply[S, E](opt: Options)(rand: TRandom)(o: Ordering[E]) =
    new TournamentSelection[S, E](o)(opt, rand)
}

class FitnessProportionateSelection[S](implicit rand: TRandom) extends Selection[S, Double] {
  // Inefficient version: recalculates distribution in every selection act. 
  def apply(pop: Seq[(S, Double)]) = {
    val distribution = Distribution.fromAnything(pop.map(_._2))
    pop(distribution(rand))
  }
  // Efficient version: Distribution calculated only once. 
  /*
  def apply[S] =
    (pop: Seq[(S, Double)]) =>
      {
        val distribution = Distribution.fromAnything(pop.map(_._2))
        (rand: TRandom) => pop(distribution(rand))
      }
      * 
      */
}

// Note: Here E stands for one objective, not entire evaluation. 
class LexicaseSelection[S, E](o: Ordering[E])(implicit rand: TRandom)
    extends StochasticSelection[S, Seq[E]](rand) {
  def apply(pop: Seq[(S, Seq[E])]) = {
    def sel(sols: Seq[(S, Seq[E])], cases: List[Int]): (S, Seq[E]) =
      if (sols.size == 1) sols(0)
      else if (cases.size == 1) sols(rand)
      else {
        val theCase = cases(rand)
        val ord = (a: (S, Seq[E]), b: (S, Seq[E])) => o.compare(a._2(theCase), b._2(theCase))
        val bestEval = BestSelector(sols, ord)
        //println("Sols:" + sols.size + " Cases: " + cases.size)
        sel(sols.filter(s => ord(bestEval, s) > 0), cases.diff(List(theCase)))
      }
    // assumes nonempty pop
    sel(pop, 0.until(pop(0)._2.size).toList)
  }
}
