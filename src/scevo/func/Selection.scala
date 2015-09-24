package scevo.func

import scevo.Distribution
import scevo.Preamble.RndApply
import scevo.evo.BestSelector
import scevo.evo.Evaluation
import scevo.evo.ScalarEvaluationMax
import scevo.tools.Options
import scevo.tools.TRandom
import scevo.evo.MultiobjectiveEvaluation

object RandomSelection {
  def apply[S, E <: Evaluation[E]](rand: TRandom) = {
    pop: Seq[(S, E)] => pop(rand)
  }
}

object GreedyBestSelection {
  def apply[S, E <: Evaluation[E]] = {
    pop: Seq[(S, E)] => BestSelector(pop)
  }
}

 object TournamentSelection {
  def apply[S, E <: Evaluation[E]](opt: Options)(rand: TRandom) = {
    val tournamentSize = opt.paramInt("tournamentSize", 7, _ >= 2)
    pop: Seq[(S, E)] => BestSelector(pop(rand, tournamentSize))
  }
}

object FitnessProportionateSelection {
  // Inefficient version: recalculates distribution in every selection act. 
  def apply[S, E <: ScalarEvaluationMax](rand: TRandom) =
    (pop: Seq[(S, E)]) =>
      {
        val distribution = Distribution.fromAnything(pop.map(_._2.v))
        pop(distribution(rand))
      }
      // Efficient version: Distribution calculated only once. 
  def apply[S, E <: ScalarEvaluationMax] =
    (pop: Seq[(S, E)]) =>
      {
        val distribution = Distribution.fromAnything(pop.map(_._2.v))
        (rand: TRandom) => pop(distribution(rand))
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
