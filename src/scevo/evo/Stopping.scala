package scevo.evo

import scevo.tools.Options

trait StoppingCondition[A <: IterativeAlgorithm[_]] extends (A => Boolean)

class MaxGenerations[A <: IterativeAlgorithm[_ <: State]](maxGenerations: Int) extends StoppingCondition[A] {
  override def apply(alg: A) = alg.currentState.iteration >= maxGenerations
}

class MaxTime[A <: IterativeAlgorithm[_]](maxMillisec: Long) extends StoppingCondition[A] {
  val startTime = System.currentTimeMillis()
  override def apply(alg: A) = timeElapsed > maxMillisec
  def timeElapsed = System.currentTimeMillis() - startTime
}

trait StoppingConditions[A <: IterativeAlgorithm[_]] {
  def stoppingConditions: List[StoppingCondition[A]]
  assert(stoppingConditions.nonEmpty, "At least one stopping condition has to be defined")
}

trait StoppingStd[A <: IterativeAlgorithm[_ <: State]] extends StoppingConditions[A] {
  this: Options =>
  def stoppingConditions = List(
    new MaxTime[A](paramInt("maxTime", 86400000, _ > 0)),
    new MaxGenerations[A](paramInt("maxGenerations", 50, _ > 0)))
}
