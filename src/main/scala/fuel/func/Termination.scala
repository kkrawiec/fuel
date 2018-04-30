package fuel.func

import fuel.util.Options
import fuel.core.StatePop
import fuel.util.Counter

object Termination {

  object MaxTime {
    def apply(opt: Options): Any => Boolean = apply(opt.paramInt("maxTime", 86400000, _ > 0))
    def apply(maxMillisec: Int): Any => Boolean = {
      val startTime = System.currentTimeMillis()
      def timeElapsed = System.currentTimeMillis() - startTime
      s: Any => timeElapsed > maxMillisec
    }
  }
  class NoImprovement[S, E] {
    def apply(ref: () => (S, E))(implicit ord: PartialOrdering[E]) = {
      (s: StatePop[(S, E)]) => !s.exists(
          es => ord.tryCompare(es._2, ref()._2).getOrElse(0) < 0)
    }
  }
  class Count {
    def apply(cnt: Counter, max: Long) = {
      s: Any => cnt.count >= max
    }
  }
  object MaxIter extends Count {
    def apply[S](cnt: Counter)(implicit opt: Options): Any => Boolean = {
      apply(cnt, opt('maxGenerations, 50, (_: Int) > 0))
    }
    def apply[S](cnt: Counter, maxGenerations: Int): Any => Boolean  = {
      super.apply(cnt, maxGenerations)
    }
  }
  def apply[S, E](otherCond: (S, E) => Boolean = (_: S, _: E) => false)(implicit config: Options) = Seq(
    MaxTime(config),
    (s: StatePop[(S, E)]) => s.exists(es => otherCond(es._1, es._2)))
}
