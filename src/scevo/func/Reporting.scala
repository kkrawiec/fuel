package scevo.func

import scevo.core.BestSelector
import scevo.tools.Collector
import scevo.tools.Options
import scevo.core.StatePop

// Reporting
class BestSoFar[S, E](opt: Options, coll: Collector, o: PartialOrdering[E])
    extends Function1[StatePop[(S, E)], StatePop[(S, E)]] {
  protected var best: Option[(S, E)] = None
  def bestSoFar: Option[(S, E)] = best
  val snapFreq = opt.paramInt("snapshot-frequency", 0)

  def apply(s: StatePop[(S, E)]) = {
    val bestOfGen = BestSelector(s.solutions, o)
    if (bestSoFar.isEmpty || o.lt(bestOfGen._2, best.get._2)) best = Some(bestOfGen)
    println(f"Gen: ${s.iteration}  BestSoFar: ${bestSoFar.get}")
    if (snapFreq > 0 && s.iteration % snapFreq == 0)
      coll.saveSnapshot(f"${s.iteration}%04d")
    s
  }
}
object BestSoFar {
  def apply[S, E](o: PartialOrdering[E])(implicit opt: Options, coll: Collector)  =
    new BestSoFar[S,E](opt, coll, o)
}

class EpilogueBestOfRun[S, E](bsf: BestSoFar[S, E], coll: Collector) 
    extends Function1[StatePop[(S, E)], StatePop[(S, E)]] {
  def apply(state: StatePop[(S, E)]) = {
    coll.setResult("lastGeneration", state.iteration)
    coll.setResult("bestOfRun.eval", 
        if (bsf.bestSoFar.isDefined) bsf.bestSoFar.get._2 else "NaN")
    coll.setResult("bestOfRun", bsf.bestSoFar.toString)
    coll.write("bestOfRun", bsf)
    state
  }
}
object EpilogueBestOfRun {
  def apply[S, E](bsf: BestSoFar[S, E])(implicit coll: Collector) = 
    new EpilogueBestOfRun[S,E](bsf, coll)
}
