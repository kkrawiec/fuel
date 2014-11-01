package scevo.evo

import scevo.tools.Options

trait StoppingCondition[A <: Algorithm[_]] extends (A => Boolean)

class MaxGenerations[A <: Algorithm[_ <: State]](maxGenerations: Int) extends StoppingCondition[A] {
  override def apply(alg: A) = alg.currentState.iteration >= maxGenerations
}

class MaxTime[A <: Algorithm[_]](maxMillisec: Long) extends StoppingCondition[A] {
  val startTime = System.currentTimeMillis()
  override def apply(alg: A) = timeElapsed > maxMillisec
  def timeElapsed = System.currentTimeMillis() - startTime
}

class BestHasProperty[ES <: EvaluatedSolution[_]](val prop: (ES => Boolean))
  extends StoppingCondition[IterativeAlgorithm[ES]] {
  var found: Option[ES] = None
  override def apply(alg: IterativeAlgorithm[ES]) = {
    val r = prop(alg.bestSoFar)
    if (r) found = Some(alg.bestSoFar)
    r
  }
}

trait StoppingConditions[A <: Algorithm[_]] {
  def stoppingConditions: List[StoppingCondition[A]]
  assert(stoppingConditions.nonEmpty, "At least one stopping condition has to be defined")
}

trait StoppingStd[A <: Algorithm[_ <: State]] extends StoppingConditions[A] {
  this: Options =>
  def stoppingConditions = List(
    new MaxTime[A](paramInt("maxTime", 86400000, _ > 0)),
    new MaxGenerations[A](paramInt("maxGenerations", 50, _ > 0)))
}
