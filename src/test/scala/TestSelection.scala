package test.scala

import fuel.func.TournamentSelection
import fuel.util.{Parsers, Random}
import org.junit.Test
import org.junit.Assert._

class TestSelection {
  @Test
  def testTournamentSelection(): Unit = {
    implicit val mockRng = new Random(new java.util.Random(1)){
      val list = Seq(4,3,2,1,0)
      var index = 0
      override def nextInt(): Int = nextInt(5)
      override def nextInt(range: Int): Int = {
        if (index >= list.size) {
          index = 0; list(index)
        }
        else {
          index += 1; list(index-1)
        }
      }
      def reset() { index = 0 }
    }

    val pop = Seq(("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5), ("f", 6))
    val ordering = Ordering[Int]

    // Elements "d" and "e" should be chosen, and "d" returned.
    // Notice that element with lesser fitness was selected. Ths is because selection
    // methods assume that lower values are better.
    assertEquals(("d", 4), new TournamentSelection[String, Int](ordering, 2).apply(pop))
    mockRng.reset()
    assertEquals(("a", 1), new TournamentSelection[String, Int](ordering, 5).apply(pop))
    mockRng.reset()

    // a negative int if this < that
    // 0 if this == that
    // a positive int if this > that
    object OrderingMaxFit extends Ordering[Int] {
      def compare(a: Int, b: Int): Int = {
        if (a > b) -1 else if (a == b) 0 else 1
      }
    }
    object OrderingMinFit extends Ordering[Int] {
      def compare(a: Int, b: Int): Int = {
        if (a > b) 1 else if (a == b) 0 else -1
      }
    }
    // Ordering decides in which direction there is preference. In FUEL there is a convention,
    // that "greater" elements in the order are worse. In the first example below,
    // we want to minimize fitness so 1 is preferred over 20 - we point to this fact by pointing
    // to 20 as "greater". This is less intuitive for MaxFit ordering, where we point to smaller
    // values as "greater".
    // ALTERNATIVE: think that values and args are: [-1, 0, 1], [a, ,b], and you point at the
    // preferred arg.

    assertEquals(-1, OrderingMinFit.compare(1, 20)) // second element "greater" (worse) than the first == preferred is first value
    assertEquals(0, OrderingMinFit.compare(1, 1))
    assertEquals(1, OrderingMinFit.compare(1, -20))

    assertEquals(1, OrderingMaxFit.compare(1, 20)) // second element "smaller" (better) than the first == preferred is second value
    assertEquals(0, OrderingMaxFit.compare(1, 1))
    assertEquals(-1, OrderingMaxFit.compare(1, -20))

    assertEquals(("d", 4), new TournamentSelection[String, Int](OrderingMinFit, 2).apply(pop))
    mockRng.reset()
    assertEquals(("a", 1), new TournamentSelection[String, Int](OrderingMinFit, 5).apply(pop))
    mockRng.reset()

    assertEquals(("e", 5), new TournamentSelection[String, Int](OrderingMaxFit, 2).apply(pop))
    mockRng.reset()
    assertEquals(("e", 5), new TournamentSelection[String, Int](OrderingMaxFit, 5).apply(pop))
    mockRng.reset()

    assertEquals(("e", 5), new TournamentSelection[String, Int](OrderingMinFit, 1).apply(pop))
    assertEquals(("d", 4), new TournamentSelection[String, Int](OrderingMinFit, 1).apply(pop))
    assertEquals(("c", 3), new TournamentSelection[String, Int](OrderingMaxFit, 1).apply(pop))
    assertEquals(("b", 2), new TournamentSelection[String, Int](OrderingMaxFit, 1).apply(pop))
    mockRng.reset()
  }
}
