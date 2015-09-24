package scevo.evo

object BestSelector {
  def apply[S, E <: Evaluation[E]](set: Seq[Tuple2[S, E]]) = 
    set.reduceLeft((a, b) => if (a._2.betterThan(b._2)) a else b)

  // I'd be happy to call this 'apply' as well, but type erasure does not permit.
  def select[E <: Evaluation[E]](s: Seq[E]) = 
    s.reduceLeft((a, b) => if (a.betterThan(b)) a else b)

  // Generic, for non-Evaluation classes
  def apply[T](set: Seq[T], better: (T, T) => Boolean) =
    set.reduceLeft((a, b) => if (better(a, b)) a else b)

  def apply[T](set: Seq[T], ord: Ordering[T]) =
    set.reduceLeft((a, b) => if (ord.compare(a, b) < 0) a else b)
}

object TestBestSelector {
  def main(args: Array[String]) =
    println(BestSelector(List(3, 1, 3, 6), Ordering[Int]))
}


/*
class ExampleSpec extends FlatSpec with Matchers {

  "A BestSelector" should "select the maximal element for maximized evaluation" in {
    BestSelector(  should be (2)
    stack.pop() should be (1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    } 
  }
}
* 
*/
