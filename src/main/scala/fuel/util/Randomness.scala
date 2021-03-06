package fuel.util

import scala.collection.generic.CanBuildFrom

/**
 * Trait intended to enable elegant use of different random number generators
 */
trait TRandom {
  def nextBoolean(): Boolean
  def nextBytes(bytes: Array[Byte])
  def nextDouble(): Double
  def nextFloat(): Float
  def nextGaussian(): Double
  def nextInt(): Int
  def nextInt(n: Int): Int
  def nextLong(): Long
  def setSeed(seed: Long)
  def shuffle[T, CC[X] <: TraversableOnce[X]](xs: CC[T])(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T]
}

/** Default random number generator - a wrapper on java.util.Random. 
 *  
 * (copied from scala source code)
 */
class Random(override val self: java.util.Random) extends scala.util.Random with TRandom {
  /** Creates a new random number generator using a single long seed. */
  def this(seed: Long) = this(new java.util.Random(seed))

  /** Creates a new random number generator using a single integer seed. */
  def this(seed: Int) = this(seed.toLong)

  /** Creates a new random number generator. */
  def this() = this(new java.util.Random())
}

object Rng {
  def apply(implicit conf: Options) = new Random(conf("seed", 1))
}

