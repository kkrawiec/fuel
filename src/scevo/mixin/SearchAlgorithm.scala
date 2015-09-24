package scevo.mixin

import scala.annotation.tailrec

import scevo.evo.Evaluation
import scevo.mixin.PopulationState
import scevo.mixin.Selection
import scevo.evo.State
import scevo.tools.Collector
import scevo.tools.Options
import scevo.tools.Randomness

trait Algorithm[S <: State] {
  def run: S
}

trait InitialState[S <: State] {
  def initialState: S
}

trait Epilogue[S <: State] {
  def epilogue(s: S) = s
}

trait IterativeAlgorithm[S <: State] extends Algorithm[S] {
  this: InitialState[S] with Step[S] with StoppingCondition[S] with Epilogue[S] =>
  override def run = {
    @tailrec def iterate(s: S): S = stop(s) match {
      case true => s
      case false  => iterate(step(s))
    }
    println("Search process started")
    val res = epilogue(iterate(initialState))
    println("Search process completed")
    res
  }
}

/* This actually hardly provides any real functionality. It's here to show the components.
 */
trait EA[S, E <: Evaluation[_]]
  extends Options with Randomness with Collector
  with InitialState[PopulationState[S, E]]
  with IterativeAlgorithm[PopulationState[S, E]]
  with SearchStepStochastic[S, E]
  with Selection[S, E]
  with Evaluator[S, E]
  with StochasticSearchOperators[S, E]
  with StoppingDefault[PopulationState[S, E]]
  with PostBestSoFar[S, E]
  with EpilogueBestOfRun[S, E]


