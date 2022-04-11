package data.structures.immutable

import munit.FunSuite

class QueueTest extends FunSuite:

  var queue: Queue[Int] = null

  override def beforeEach(context: BeforeEach): Unit =
    queue = Queue(2, 3, 5)

  test(".length returns the correct length of queue.") {
    assertEquals(queue.length, 3)
  }

  test(".enqueue(x) adds an item to the queue.") {
    val q = queue.enqueue(7)
    assertEquals(q.length, 4)
  }

  test(".dequeue returns the first item of the queue") {
    assertEquals(queue.dequeue, Some(5))
  }

  test("Empty queue has length zero.") {
    assertEquals(Queue.empty.length, 0)
  }

end QueueTest
