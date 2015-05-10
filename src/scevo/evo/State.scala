package scevo.evo

import scevo.tools.Options

/* Search state. 
 */

trait State extends Serializable {
  def iteration: Int
}

/* Note: Population is a Seq, so duplicates are allowed.
 */
trait PopulationState[S <: Solution, E <: Evaluation] extends State {
  def solutions: Seq[EvaluatedSolution[S, E]]
}

object PopulationState {
  def apply[S <: Solution, E <: Evaluation](sols: Seq[EvaluatedSolution[S, E]], iter: Int = 0): PopulationState[S, E] =
    new PopulationState[S, E] {
      require(sols.size > 0, "The set of working solutions in a state cannot be empty")
      override val solutions = sols
      override val iteration = iter
    }

  def init[S <: Solution, E <: Evaluation](popSize: Int,
    genSolution: () => EvaluatedSolution[S, E]): PopulationState[S, E] = {
    PopulationState(for (i <- 0 until popSize) yield genSolution(), 0)
  }
}

object EmptyState {
  def apply() = new State { override def iteration = 0 }
}

// RandomStateGenerator?
trait InitialPopulationState[S <: Solution, E <: Evaluation]
  extends InitialState[PopulationState[S, E]] {
  this: Options with SeparableEvaluator[S, E] =>
  val populationSize = paramInt("populationSize", 1000, _ > 0)
  def randomSolution: S
  override def initialState = PopulationState.init(populationSize,
    () => { val r = randomSolution; ESol(r, evaluate(r)) })
}

/* Archive is a set, so no duplicates allowed
 */
trait Archive[S <: Solution, E <: Evaluation] extends Serializable {
  def archive: Set[EvaluatedSolution[S, E]]
}

trait PopStateWithArchive[S <: Solution, E <: Evaluation]
  extends PopulationState[S, E]
  with Archive[S, E]
  with Serializable 