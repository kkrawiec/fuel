package scevo.func.example

import scala.collection.immutable.BitSet

import scevo.func.Experiment
import scevo.func.SimpleEA
import scevo.moves.BitSetMoves
import scevo.tools.OptCollRng

/**
  * Use case: MaxOnes with GA.
  *
  * Actually implemented as MinOnes (i.e., all bits should be zeroed). 
  * 
  * Try it in Scala REPL; assuming binaries in directory 'bin', this should work:
  * $ scala -cp bin
  * scala> scevo.func.example.MaxOnes.main()
  * scala> scevo.func.example.MaxOnes.main(Array("--numVars", "50")) 
  * scala> scevo.func.example.MaxOnes.main(Array("--numVars 50"))
  * 
  */
object MaxOnes {
  def main(args: Array[String] = Array("--numVars", "500", "--maxGenerations", "200")) {
    implicit val (opt, coll, rng) = OptCollRng(args)

    val ga = new SimpleEA(
      moves = BitSetMoves(opt.paramInt("numVars", _ > 0)),
      eval = (s: BitSet) => s.size,
      stop = (s: BitSet, e: Int) => e == 0)

    Experiment.run(ga)
  }
}
