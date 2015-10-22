package scevo.func

import scala.collection.parallel.ForkJoinTaskSupport

import scevo.core.StatePop

/** This trait is mostly in order to get around type erasure in some calls. 
 *  
 */
trait Evaluation[S, E] extends Function1[StatePop[S], StatePop[(S, E)]]

/**
  * Evaluates population solution by solution.
  *
  */
object SequentialEval {
  def apply[S, E](f: S => E) = new Evaluation[S, E] {
    override def apply(s: StatePop[S]) = StatePop(s.map(x => (x, f(x))))
  }
}

/**
  * Evaluates population in parallel.
  *  The second version allows controlling the number of threads.
  *
  */
object ParallelEval {
  def apply[S, E](f: S => E) = new Evaluation[S, E] {
    override def apply(s: StatePop[S]) = StatePop(s.par.map(x => (x, f(x))).to)
  }
  def apply[S, E](f: S => E, parLevel: Int) = new Evaluation[S, E] {
    val ts = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(parLevel))
    override def apply(s: StatePop[S]) = StatePop({
      val c = s.par
      c.tasksupport = ts
      c.map(x => (x, f(x))).to
    })
  }
}