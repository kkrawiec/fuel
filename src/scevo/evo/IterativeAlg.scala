package scevo.evo

/* 
 * Iterative search algorithm, with every iteration implemented as SearchStep
 */

trait IterativeAlgorithm[ES <: EvaluatedSolution[_ <: Evaluation]] {
  def currentState: State[ES]
  // Determining the best in population can be costly for large populations, hence this field
  def bestSoFar: ES
  def searchStep: SearchStep[ES]
  def stopConditions: Seq[StoppingCondition[ES]]
  def apply(postGenerationCallback: (IterativeAlgorithm[ES] => Unit) = ((_ : IterativeAlgorithm[ES]) => ())): State[ES]
}

class Evolution[S <: Solution, ES <: EvaluatedSolution[_ <: Evaluation]](val initialState: State[ES],
  val searchStep: SearchStep[ES],
  val stopConditions: Seq[StoppingCondition[ES]])
  extends IterativeAlgorithm[ES] {
  private var current: State[ES] = initialState
  override def currentState = current

  private var best = BestSelector(initialState.solutions).asInstanceOf[ES] // Need this :(
  override def bestSoFar = best

  var log = List[String]()
  def log(s: String) {
    val sg = "Generation: " + currentState.iteration + s
    println(sg)
    log = log :+ sg
  }

  /* Performs evolutionary run. 
   * Returns the final state of evolutionary process, the best of run solution, and the ideal solution (if found). 
   * PROBABLY can be called multiple times on the same Evolution; that should continue search. 
   *   */
  override def apply(postGenerationCallback: (IterativeAlgorithm[ES] => Unit) = ((_ : IterativeAlgorithm[ES]) => ())): State[ES] = {

    assert(!stopConditions.isEmpty, "At least one stopping condition has to be defined")

    println("Search process started")
    do {
      var nextStep = searchStep(Seq(current))
      current = if (nextStep.isEmpty) {
        log("None of candidate solutions passed the evaluation stage. Restarting. ")
        State[ES](initialState.solutions, current.iteration + 1)
      } else {
        val state = nextStep.get
        val bestOfGen = BestSelector(state.solutions).asInstanceOf[ES]
        if (bestOfGen.eval.betterThan(best.eval)) best = bestOfGen
        state
      }
      postGenerationCallback(this)
    } while (stopConditions.forall(sc => !sc(this)))

    println("Search process completed")
    current
  }
}

