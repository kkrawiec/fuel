package scevo.evo

import scevo.tools.Options

/* Search state. 
 */

trait State extends Serializable {
  def iteration: Int
}

class PopulationState[S <: Solution, E <: Evaluation](val solutions: Seq[EvaluatedSolution[S, E]], 
    override val iteration: Int)
  extends State {
  require(solutions.size > 0, "The set of working solutions in a state cannot be empty")
}

object PopulationState {

  def apply[S <: Solution, E <: Evaluation](solutions: Seq[EvaluatedSolution[S, E]], iteration: Int = 0) =
    new PopulationState[S, E](solutions, iteration)

  def init[S <: Solution, E <: Evaluation](popSize: Int,
    genSolution: () => EvaluatedSolution[S, E]): PopulationState[S, E] = {
    require(popSize > 0, "Population cannot be empty")
    PopulationState(for (i <- 0 until popSize) yield genSolution(), 0)
  }
}

trait InitialPopulationState[S <: Solution, E <: Evaluation]
  extends InitialState[PopulationState[S, E]] {
  this: Options with SeparableEvalutator[S, E] =>
  val populationSize = paramInt("populationSize", 1000, _ > 0)
  def randomSolution: S
  override def initialState = PopulationState.init(populationSize,
    () => { val r = randomSolution; ESol(r, evaluate(r)) })
}
