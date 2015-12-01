package scevo.func

import scala.annotation.tailrec

import scevo.core.StatePop
import scevo.util.Options

/**
  * Component factories
  *
  * Note that IterativeAlgorithm is in general agnostic about evaluation.
  */

object Iteration {
  def apply[S](step: S => S)(stop: Seq[S => Boolean]): S => S = {
    @tailrec def iterate(s: S): S = stop.forall((sc: S => Boolean) => !sc(s)) match {
      case false => s
      case true  => iterate(step(s))
    }
    iterate
  }
}

/** Initializer is any function that can create a State 'out of nothing'
 *  (e.g., at random). 
 *  
 *  Must be defined as Unit => State, because ()=> State is not composable 
 *  (has no andThen method)
 */
trait Initializer[S] extends Function1[Unit, S]

class RandomStatePop[S](solutionGenerator: () => S)(implicit opt: Options)
    extends Initializer[StatePop[S]] {
  val populationSize = opt('populationSize, 1000, (_:Int) > 0)
  def apply(x: Unit) = StatePop(Seq.fill(populationSize)(solutionGenerator()))
}

object RandomStatePop {
  def apply[S](solutionGenerator: () => S)(implicit opt: Options) =
    new RandomStatePop(solutionGenerator)(opt)
}
