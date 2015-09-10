package scevo.evo

/* Search state. 
 */

trait State extends Serializable {
  def iteration: Int
}

/* Note: Population is a Seq, so duplicates are allowed.
 */
trait PopulationState[S, E <: Evaluation] extends State {
  def solutions: Seq[EvaluatedSolution[S, E]]
}

object PopulationState {
  def apply[S, E <: Evaluation](sols: Seq[EvaluatedSolution[S, E]], iter: Int = 0): PopulationState[S, E] =
    new PopulationState[S, E] {
      require(sols.size > 0, "The set of working solutions in a state cannot be empty")
      override val solutions = sols
      override val iteration = iter
    }

  def init[S, E <: Evaluation](popSize: Int,
    genSolution: () => EvaluatedSolution[S, E]): PopulationState[S, E] = {
    PopulationState(for (i <- 0 until popSize) yield genSolution(), 0)
  }
}

object EmptyState {
  def apply() = new State { override def iteration = 0 }
}

