package scevo.evo

import scevo.tools.Options

trait StoppingCondition[S <: State] {
  def stop(s: S) = false
}

trait MaxGenerations[S <: State] extends StoppingCondition[S] {
  this: Options =>
  val maxGenerations = paramInt("maxGenerations", 50, _ > 0)
  override def stop(s: S) = s.iteration >= maxGenerations || super.stop(s)
}

trait MaxTime[S <: State] extends StoppingCondition[S] {
  this: Options =>
  val maxMillisec = paramInt("maxTime", 86400000, _ > 0)
  val startTime = System.currentTimeMillis()
  def timeElapsed = System.currentTimeMillis() - startTime
  override def stop(s: S) = timeElapsed > maxMillisec || super.stop(s)
}

trait StoppingDefault[S <: State] extends MaxGenerations[S] with MaxTime[S] {
  this: Options =>
}
