package scevo.mixin

import scevo.tools.Options
import scevo.evo.Evaluation

trait InitialPopulationState[S, E <: Evaluation]
  extends InitialState[PopulationState[S, E]] {
  this: Options with SeparableEvaluator[S, E] =>
  val populationSize = paramInt("populationSize", 1000, _ > 0)
  def randomSolution: S
  override def initialState = PopulationState.init(populationSize,
    () => { val r = randomSolution; ESol(r, evaluate(r)) })
}

/* Archive is a set, so no duplicates allowed
 */
trait Archive[S, E <: Evaluation] extends Serializable {
  def archive: Set[EvaluatedSolution[S, E]]
}

trait PopStateWithArchive[S, E <: Evaluation]
  extends PopulationState[S, E]
  with Archive[S, E]
  with Serializable 