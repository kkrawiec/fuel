package scevo.func

import scala.annotation.tailrec

import scevo.evo.State
import scevo.tools.Options

/**
  * Scevo-like functionality in functional programming style
  *
  * Component factories
  * Note that IterativeAlgorithm is in general agnostic about evaluation.
  */

object Iteration {
  def apply[S <: State](step: S => S)(stop: Seq[S => Boolean]): S => S = {
    @tailrec def iterate(s: S): S = stop.forall((sc: S => Boolean) => !sc(s)) match {
      case false => s
      case true  => iterate(step(s))
    }
    iterate
  }
  // Version for population-based algorithms, 
  /*
  def apply[S, E](step: StatePop[(S, E)] => StatePop[(S, E)])(
        stop: Seq[StatePop[(S, E)] => Boolean]) : StatePop[(S,E)] => StatePop[(S,E)] = {
    apply(step)(stop) 
  }
  * 
  */
}

// Must be defined as Unit => State, because ()=> State is not composable (no andThen method)
trait Initializer[S <: State] extends Function1[Unit, S]

class RandomStatePop[S](solutionGenerator: () => S)(implicit opt: Options)
    extends Initializer[StatePop[S]] {
  val populationSize = opt.paramInt("populationSize", 1000, _ > 0)
  def apply(x: Unit) = Population(for (i <- 0 until populationSize) yield solutionGenerator())
}

object RandomStatePop {
  def apply[S](solutionGenerator: () => S)(implicit opt: Options) =
    new RandomStatePop(solutionGenerator)(opt)
}