package scevo.evo

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

  val maxGenerations = options.getOrElse("maxGenerations", "50").toInt
  assert(maxGenerations > 0, "Number of generations should be > 0")

  val maxTime = options.getOrElse("maxTime", "86400000").toLong
  assert(maxTime > 0, "MaxTime should be > 0")

  val scMaxTime = new MaxTime[A](maxTime)
  val scMaxGeneration = new MaxGenerations[A](maxGenerations)

  def stoppingConditions = List(scMaxTime, scMaxGeneration)
}

