package scevo.func

import scevo.tools.Options
import scala.annotation.tailrec
import scevo.evo.State


/** Scevo-like functionality in functional programming style
 * 
 * Component factories
 * Note that IterativeAlgorithm is in general agnostic about evaluation.
 */

object IterativeAlgorithm {
  def apply[S <: State](step: S => S)(stop: Seq[S => Boolean]): S => S = {
    @tailrec def iterate(s: S): S = stop.forall((sc: S => Boolean) => !sc(s)) match {
      case false => s
      case true  => iterate(step(s))
    }
    iterate
  }
  // Version for population-based algorithms, 
  // with default best-so-far and best-of-run reporting
  def apply[S, E](
    env: Environment)(
      step: StatePop[(S, E)] => StatePop[(S, E)])(
        stop: Seq[StatePop[(S, E)] => Boolean])( 
        o: PartialOrdering[E]): StatePop[(S, E)] => StatePop[(S, E)] = {
    val bsf = new BestSoFar[S, E]
    def reporting = bsf(env, env, o)
    apply(step andThen reporting)(stop) andThen EpilogueBestOfRun(bsf, env)
  }
}


object RandomStatePop {
  def apply[S](opt: Options, solutionGenerator: () => S) = {
    val populationSize = opt.paramInt("populationSize", 1000, _ > 0)
    _: Unit => Population(for (i <- 0 until populationSize) yield solutionGenerator())
  }
}