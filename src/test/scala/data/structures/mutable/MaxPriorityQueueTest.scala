package data.structures.mutable

import munit.FunSuite

class MaxPriorityQueueTest extends FunSuite:
  var queue: MaxPriorityQueue[Int] = null
  override def beforeEach(context: BeforeEach): Unit =
    queue = MaxPriorityQueue(23, 11, 19, 7, 19, 5)

  test(".size returns the correct count of items.") {
    assertEquals(queue.size, 6)
  }

  test(".isEmpty returns false value iff queue is non-empty.") {
    assert(!queue.isEmpty)
  }

  test(".max() returns the current maximum value.") {
    assertEquals(queue.max(), Some(23))
  }

  test("The size of the queue decreases when calling .max()") {
    val prevSize = queue.size
    queue.max()
    assertEquals(queue.size, prevSize - 1)
  }
end MaxPriorityQueueTest
