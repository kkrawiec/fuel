package scevo.mixin

import scevo.evo.Evaluation
import scevo.tools.Collector
import scevo.tools.Options
import scevo.tools.Randomness
import scevo.evo.BestSelector
import scevo.evo.State

/*
 * A single step of an iterative search algorithm. step() is supposed to carry out one iteration. 
 * A search operator returns a list of of candidate solutions; possibly empty (if, e.g., feasibility conditions are not met).
 * 
 * Search steps can be combined (chained) via mixins, by calling super.step() 
 * In such a case, the state is being passed from one Step trait to another, 
 * and this happens *within each iteration of search*.
 * 
 * Also post-iteration reporting is realized as a Step
 */
trait Step[S <: State] {
  def step(s: S) = s
}

trait SearchStepStochastic[S, E <: Evaluation[_]]
  extends Step[PopulationState[S, E]] {
  this: StochasticSearchOperators[S, E] with Selection[S, E] with Evaluator[S, E] with Randomness =>
  override def step(state: PopulationState[S, E]) = {
    val s = super.step(state)
    val source = selector(Seq(s))
    var offspring = scala.collection.mutable.MutableList[S]()
    // Note: This loop will iterate forever is none of the search operators manages to produce a solution. 
    while (offspring.size < source.numSelected)
      offspring ++= operator(rng)(source)
    val evaluated = apply(offspring.toList)
    PopulationState(evaluated, s.iteration + 1)
  }
}

trait PostBestSoFar[S, E <: Evaluation[E]] extends Step[PopulationState[S, E]] {
  this: Options with Collector with Step[PopulationState[S, E]] =>
  protected var best: Option[EvaluatedSolution[S, E]] = None
  def bestSoFar: Option[EvaluatedSolution[S, E]] = best

  val snapFreq = paramInt("snapshot-frequency", 0)

  override def step(state: PopulationState[S, E]) = {
    val s = super.step(state)
    val bestOfGen = BestSelectorES(s.solutions)
    if (bestSoFar.isEmpty || bestOfGen.eval.betterThan(best.get.eval)) best = Some(bestOfGen)
    println(f"Gen: ${s.iteration}  BestSoFar: ${bestSoFar.get}")
    if (snapFreq > 0 && s.iteration % snapFreq == 0)
      rdb.saveSnapshot(f"${s.iteration}%04d")
    s
  }
}

trait EpilogueBestOfRun[S, E <: Evaluation[E]] extends Epilogue[PopulationState[S, E]] {
  this: Collector with PostBestSoFar[S, E] =>
  override def epilogue(state: PopulationState[S, E]) = {
    val s = super.epilogue(state)
    rdb.setResult("lastGeneration", s.iteration)
    rdb.setResult("bestOfRun.fitness", bestSoFar.get.eval)
    rdb.setResult("bestOfRun.genotype", bestSoFar.toString)
    //rdb.write("bestOfRun", bestSoFar)
    s
  }
}
