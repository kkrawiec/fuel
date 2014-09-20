package scevo.evo

trait StoppingCondition[ES <: EvaluatedSolution[_ <: Evaluation]] extends (IterativeAlgorithm[ES] => Boolean)

class MaxGenerations[ES <: EvaluatedSolution[_ <: Evaluation]](maxGenerations: Int) extends StoppingCondition[ES] {
  override def apply(alg: IterativeAlgorithm[ES]) = alg.currentState.iteration >= maxGenerations
}

class BestHasProperty[ES <: EvaluatedSolution[_ <: Evaluation]](val prop: (ES => Boolean)) extends StoppingCondition[ES] {
  var found: Option[ES] = None
  override def apply(alg: IterativeAlgorithm[ES]) = {
    val r = prop(alg.bestSoFar)
    if (r) found = Some(alg.bestSoFar)
    r
  }
}

class MaxTime[ES <: EvaluatedSolution[_ <: Evaluation]](maxMillisec: Long) extends StoppingCondition[ES] {
  val startTime = System.currentTimeMillis()
  override def apply(alg: IterativeAlgorithm[ES]) = timeElapsed > maxMillisec
  def timeElapsed = System.currentTimeMillis() - startTime
}


