package scevo.func

import scevo.Preamble.RndApply
import scevo.evo.BestSelector
import scevo.evo.Evaluation
import scevo.evo.MultiobjectiveEvaluation
import scevo.tools.Options
import scevo.tools.TRandom
import scevo.Distribution
import scevo.evo.ScalarEvaluationMax

object TournamentSelection {
  def apply[S, E <: Evaluation](opt: Options)(rand: TRandom) = {
    val tournamentSize = opt.paramInt("tournamentSize", 7, _ >= 2)
    pop: Seq[(S, E)] => BestSelector(pop(rand, tournamentSize))
  }
}

object FitnessProportionateSelection {
  def apply[S, E <: ScalarEvaluationMax](opt: Options)(rand: TRandom) = 
    (pop: Seq[(S, E)]) =>
      {
        val distribution = Distribution.fromAnything(pop.map(_._2.v))
        pop(distribution(rand))
      }
}

object LexicaseSelection {
  def apply[S, E <: MultiobjectiveEvaluation](rand: TRandom) = {
    def sel(sols: Seq[(S, E)], cases: List[Int]): (S, E) =
      if (sols.size == 1)
        sols(0)
      else if (cases.size == 1)
        sols(rand)
      else {
        val theCase = cases(rand)
        val bestEval = BestSelector.select(sols.map(_._2.v(theCase)))
        //println("Sols:" + sols.size + " Cases: " + cases.size)
        sel(sols.filter(s => !bestEval.betterThan(s._2.v(theCase))), cases.diff(List(theCase)))
      }
    // assumes nonempty pop
    pop: Seq[(S, E)] => sel(pop, 0.until(pop(0)._2.size).toList)
  }
}

object RandomSelection {
  def apply[S, E <: Evaluation](rand: TRandom) = {
    pop: Seq[(S, E)] => pop(rand)
  }
}