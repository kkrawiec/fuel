package scevo.evo

// Interaction outcomes are always Doubles
trait InteractionFunction[A, B] extends Function2[A, B, Double] {
  def interact(a: A, b: B) = apply(a, b) // alias
}

trait AdditiveInteractionF[A, B] extends InteractionFunction[A, B] {
  def apply(a: A, bs: Seq[B]): Double = bs.map(b => apply(a, b)).sum
}

trait EqInteraction[A] extends InteractionFunction[A, A] {
  // Note the convention: equals => *zero*
  def apply(a: A, b: A) = if (a equals b) 0 else 1
}
trait L1Interaction extends InteractionFunction[Double, Double] {
  // Note the convention: equals => *zero*
  def apply(a: Double, b: Double) = math.abs(a - b)
}
trait L2Interaction extends InteractionFunction[Double, Double] {
  // Note the convention: equals => *zero*
  def apply(a: Double, b: Double) = (a - b) * (a - b)
}
trait SameSignInteraction extends InteractionFunction[Double, Double] {
  // Note the convention: equals => *zero*
  def apply(a: Double, b: Double) =
    if (a >= 0 && b >= 0) 0
    else if (a < 0 && b < 0) 0 else 1
}
