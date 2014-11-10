package scevo.evo

import scevo.tools.Logging
import scevo.tools.Options
import scevo.tools.Randomness
import scevo.tools.ResultDatabase

trait Algorithm[S <: State] {
  def run(rdb: ResultDatabase): S
}

trait IterativeAlgorithm[S <: State] extends Algorithm[S] {
  this: InitialState[S] =>
  def currentState: S
}

trait PopulationAlgorithm[ES <: EvaluatedSolution[_]] extends IterativeAlgorithm[PopulationState[ES]] {
  this: InitialState[PopulationState[ES]] =>
  override def currentState: PopulationState[ES]
}

trait PostIterationAction[ES <: EvaluatedSolution[_ <: Evaluation]] {
  def postIteration: Unit
}

trait PostBestSoFar[ES <: EvaluatedSolution[_ <: Evaluation]] extends PostIterationAction[ES] {
  this: PopulationAlgorithm[ES] =>
  private var best: Option[ES] = None
  def bestSoFar: Option[ES] = best
  override def postIteration: Unit = {
    val bestOfGen = BestSelector(currentState.solutions)
    if (bestSoFar.isEmpty || bestOfGen.eval.betterThan(best.get.eval)) best = Some(bestOfGen)
    println(f"Generation: ${currentState.iteration}  BestSoFar: ${bestSoFar.get.eval}")
  }
}

trait Epilogue[ES <: EvaluatedSolution[_ <: Evaluation]] {
  def epilogue(rdb: ResultDatabase): Unit
}

trait EpilogueBestOfRun[ES <: EvaluatedSolution[_ <: Evaluation]] extends Epilogue[ES] {
  this: PostBestSoFar[ES] =>
  def epilogue(rdb: ResultDatabase): Unit = {
    rdb.setResult("bestOfRun.fitness", bestSoFar.get.eval)
    rdb.setResult("bestOfRun.genotype", bestSoFar.toString)
  }
}

/* 
 * Iterative search algorithm, with every iteration implemented as SearchStep
 */
trait Evolution[S <: Solution, ES <: EvaluatedSolution[_ <: Evaluation]]
  extends PopulationAlgorithm[ES] with Logging {
  this: SearchStepStochastic[S, ES] with StoppingConditions[PopulationAlgorithm[ES]] with PostIterationAction[ES] with InitialState[PopulationState[ES]] with Epilogue[ES] =>

  private var current: PopulationState[ES] = _
  override def currentState = current

  /* Performs evolutionary run. 
   * Returns the final state of evolutionary process, the best of run solution, and the ideal solution (if found). 
   * PROBABLY can be called multiple times on the same Evolution; that should continue search. 
   *   */
  override def run(rdb: ResultDatabase): PopulationState[ES] = {
    current = initialState
    println("Search process started")
    do {
      var nextStep = apply(Seq(current))
      current = if (nextStep.isEmpty) {
        log(s"Generation ${current.iteration}: None of candidate solutions passed the evaluation stage. Restarting. ")
        PopulationState(initialState.solutions, current.iteration + 1)
      } else
        nextStep.get
      postIteration
    } while (stoppingConditions.forall(sc => !sc(this)))
    println("Search process completed")
    rdb.setResult("lastGeneration", current.iteration)
    epilogue(rdb)
    current
  }
}

trait EA[S <: Solution, ES <: EvaluatedSolution[_ <: Evaluation]]
  extends Evolution[S, ES]
  with Options with Randomness
  with InitialState[PopulationState[ES]]
  with SearchStepStochastic[S, ES]
  with Selection[ES]
  with Evaluator[S, ES]
  with StochasticSearchOperators[ES, S]
  with StoppingConditions[PopulationAlgorithm[ES]]
  with PostBestSoFar[ES]
  with EpilogueBestOfRun[ES] 
