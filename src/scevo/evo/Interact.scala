package scevo.evo

// Interaction outcomes are always Doubles
trait InteractionFunction[A, B] extends Function2[A, B, Double] 

trait AdditiveInteractionF[A, B] extends InteractionFunction[A, B] {
  def apply(a: A, bs: Seq[B]): Double = bs.map(b => apply(a, b)).sum
}

trait EqInteraction[A] extends InteractionFunction[A, A] {
  // Note the convention: equals => *zero*
  def apply(a: A, b: A) = if(a equals b) 0 else 1
}
trait L1Interaction extends InteractionFunction[Double, Double] {
  def apply(a: Double, b: Double) = math.abs(a - b)
}
trait L2Interaction extends InteractionFunction[Double, Double] {
  def apply(a: Double, b: Double) = (a - b)*(a - b)
}
