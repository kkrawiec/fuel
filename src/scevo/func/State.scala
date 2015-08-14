package scevo.func

import scevo.evo.State

trait StatePop[T] extends State {
  def solutions: Seq[T]
}

object StatePop {
  def apply[T](sols: Seq[T], iter: Int = 0) = new StatePop[T] {
    require(sols.size > 0, "The set of working solutions in a state cannot be empty")
    override val solutions = sols
    override val iteration = iter
  }
  def apply[T](s1: StatePop[T], s2: StatePop[T]) = new StatePop[T] {
    override val solutions = s1.solutions ++ s2.solutions
    override val iteration = math.max(s1.iteration, s2.iteration)
  }
}
