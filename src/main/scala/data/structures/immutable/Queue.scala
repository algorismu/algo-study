package data.structures.immutable

/** Queues support retrieval of items in first-in, first-out (FIFO) order.
  *
  * @tparam A
  *   type of items in Queue.
  *
  * @param items
  *   list of items recently joined the Queue
  *
  * @param queue
  *   list of items ready to be processed from Queue.
  */
class Queue[+A] private (
    private val items: List[A],
    private val queue: List[A]
):

  /** Add an item at the end of the `Queue`
    * @tparam B
    *   type of the item add
    *
    * @param item
    *   the item to added to the end of the `Queue`
    *
    * @return
    *   a new `Queue` that contains added item.
    */
  def enqueue[B >: A](item: B): Queue[B] =
    new Queue(item :: items, queue)

  /** Retrieve the first item in the `Queue`
    *
    * @tparam B
    *   type of the item to be retrieved
    *
    * @return
    *   an optional value that contains the first item.
    */
  def dequeue[B >: A]: Option[B] =
    if isEmpty
    then None
    else Some(mirror.queue.head)

  /** Checks the length of the `Queue`
    *
    * @return
    *   length of the `Queue` if non-empty, zero if empty.
    */
  def length: Int = queue.length + items.length

  /** Retrieves the rest of the `Queue` with first item execluded.
    *
    * @tparam B
    *   type of items in the rest of the `Queue`
    *
    * @return
    *   rest of the Queue.
    */
  def rest[B >: A]: Queue[B] =
    if isEmpty
    then this
    else new Queue(items, queue.tail)

  /** Checks if the queue is empty
    *
    * @return
    *   true if the `Queue` is empty, otherwise false.
    */
  def isEmpty: Boolean = queue.isEmpty && items.isEmpty

  /** Prepares items for retrieval order (FIFO) by reversing enqueued items.
    */
  private def mirror: Queue[A] =
    if queue.isEmpty
    then new Queue(Nil, items.reverse)
    else this

end Queue

/** Queue object contains utility methods for constructing `Queue`
  */
object Queue:
  def apply[A](items: A*): Queue[A] = new Queue(Nil, items.toList.reverse)
  def empty[A]: Queue[A] = new Queue(Nil, Nil)
