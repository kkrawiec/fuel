package scevo.func

import scevo.evo.State
import scevo.tools.Options

object Termination {
  object MaxIter {
    def apply[S <: State](opt: Options) = {
      val maxGenerations = opt.paramInt("maxGenerations", 50, _ > 0)
      s: S => s.iteration >= maxGenerations
    }
  }
  object MaxTime {
    def apply(opt: Options) = {
      val maxMillisec = opt.paramInt("maxTime", 86400000, _ > 0)
      val startTime = System.currentTimeMillis()
      def timeElapsed = System.currentTimeMillis() - startTime
      s: Any => timeElapsed > maxMillisec
    }
  }
  def apply[S, E](otherCond: (S, E) => Boolean = (_: S, _: E) => false)(implicit config: Options) = Seq(
    MaxIter[StatePop[(S, E)]](config),
    MaxTime(config),
    (s: StatePop[(S, E)]) => s.solutions.exists(es => otherCond(es._1, es._2)))
}
