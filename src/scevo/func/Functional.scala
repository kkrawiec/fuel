package scevo.func

import scevo.evo.BestES
import scevo.tools.Collector
import scevo.tools.Options

// Reporting
class BestSoFar[S, E](opt: Options, coll: Collector, o: PartialOrdering[E])
    extends Step[(S, E)] {
  protected var best: Option[(S, E)] = None
  def bestSoFar: Option[(S, E)] = best
  val snapFreq = opt.paramInt("snapshot-frequency", 0)

  override def apply(s: StatePop[(S, E)]) = {
    val bestOfGen = BestES(s.solutions, o)
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
    extends Step[(S, E)] {
  def apply(state: StatePop[(S, E)]) = {
    coll.setResult("lastGeneration", state.iteration)
    coll.setResult("bestOfRun.fitness", if (bsf.bestSoFar.isDefined) bsf.bestSoFar.get._2 else "NaN")
    coll.setResult("bestOfRun.genotype", bsf.bestSoFar.toString)
    coll.write("bestOfRun", bsf.bestSoFar)
    state
  }
}
object EpilogueBestOfRun {
  def apply[S, E](bsf: BestSoFar[S, E])(implicit coll: Collector) = new EpilogueBestOfRun[S,E](bsf, coll)
}

  /*
  def apply[S <: State](init: Unit => S)(step: S => S)(stop: Seq[S => Boolean])(epilogue: S => S) : Unit => S = 
  def apply[S <: State](init: Unit => S)(step: S => S)(stop: S => Boolean*)(epilogue: S => S) : Unit => S = {
    apply(init)(step)(stop)(epilogue)
    */
  /*
  def apply[S <: State](init: Unit => S)(step: S => S)(stop: Seq[S => Boolean])(epilogue: S => S): Unit => S = {
    @tailrec def iterate(s: S): S = stop.forall((sc: S => Boolean) => !sc(s)) match {
      case false => s
      case true  => iterate(step(s))
    }
    init andThen iterate andThen epilogue
  }
  * 
  */
/*
  def apply[S <: Solution, E <: Evaluation](solutionBuilder: () => (Seq[Tuple2[S, E]] => List[S]),
    targetSize: Int) = {
    current: StatePop[Tuple2[S, E]] =>
      @tailrec def breed(next: List[S]): Seq[S] =
        if (next.size >= targetSize)
          next.take(targetSize)
        else
          breed(solutionBuilder()(current.solutions) ++ next)
      StatePop[S](breed(List[S]()), current.iteration + 1)
  }
  * 
  */