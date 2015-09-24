package scevo.func

import scevo.Distribution
import scevo.Preamble.RndApply
import scevo.evo.BestSelector
import scevo.tools.Options
import scevo.tools.TRandom
import scevo.evo.BestES

object RandomSelection {
  def apply[S, E](rand: TRandom) = {
    pop: Seq[(S, E)] => pop(rand)
  }
}

object GreedyBestSelection {
  def apply[S, E](o: Ordering[E]) = {
    pop: Seq[(S, E)] => BestES(pop, o)
  }
}

object TournamentSelection {
  def apply[S, E](opt: Options)(rand: TRandom)(o: Ordering[E]) = {
    val tournamentSize = opt.paramInt("tournamentSize", 7, _ >= 2)
    pop: Seq[(S, E)] => BestES(pop(rand, tournamentSize), o)
  }
}

object FitnessProportionateSelection {
  // Inefficient version: recalculates distribution in every selection act. 
  def apply[S](rand: TRandom) =
    (pop: Seq[(S, Double)]) =>
      {
        val distribution = Distribution.fromAnything(pop.map(_._2))
        pop(distribution(rand))
      }
  // Efficient version: Distribution calculated only once. 
  def apply[S] =
    (pop: Seq[(S, Double)]) =>
      {
        val distribution = Distribution.fromAnything(pop.map(_._2))
        (rand: TRandom) => pop(distribution(rand))
      }
}

// Note: Here E stands for one objective, not entire evaluation. 
object LexicaseSelection {
  def apply[S, E](rand: TRandom)(o: Ordering[E]) = {
    def sel(sols: Seq[(S, Seq[E])], cases: List[Int]): (S, Seq[E]) =
      if (sols.size == 1)
        sols(0)
      else if (cases.size == 1)
        sols(rand)
      else {
        val theCase = cases(rand)
        val ord = (a: (S, Seq[E]), b: (S, Seq[E])) => o.compare(a._2(theCase), b._2(theCase)) 
        val bestEval = BestSelector(sols, ord)
        //println("Sols:" + sols.size + " Cases: " + cases.size)
        sel(sols.filter(s => ord(bestEval, s) > 0), cases.diff(List(theCase)))
      }
    // assumes nonempty pop
    pop: Seq[(S, Seq[E])] => sel(pop, 0.until(pop(0)._2.size).toList)
  }
}
