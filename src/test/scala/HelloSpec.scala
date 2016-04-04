import collection.mutable.Stack
import org.scalatest._

object Collection extends Tag("com.mycompany.tags.Collection")

// @Ignore
class ExampleSpec extends FlatSpec with Matchers {

  "A Stack" should "pop values in last-in-first-out order" taggedAs(Collection) in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    info("Pushed to stack")
    stack.pop() should be (2)
    stack.pop() should be (1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }

  "A List" must "have a cons operation" in {
    1 :: List(2,3) should be (List(1,2,3))
  }

  ignore must "have a nil value" in {
    1 :: Nil should be (List(1))
  }

  "An empty List" should "have size 0" in (pending)
}
