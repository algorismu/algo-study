package data.structures.mutable

import collection.mutable.ArrayBuffer
import math.Ordering.Implicits.infixOrderingOps
import PriorityQueueUtil.Position

/** A `MaxPriorityQueue` is an abstract data type that allows more flexibility
  * in inserting new items at arbitrary intervals.
  *
  * @tparam A
  *   type of items in `MaxPriorityQueue`.
  */
case class MaxPriorityQueue[A: Ordering] private (
    private var heap: ArrayBuffer[A] = ArrayBuffer.empty[A]
):

  /** Inserts an item in the `Queue` considring its priority value.
    *
    * @param item
    *   the item to be inserted.
    */
  def insert(item: A): Unit =
    heap = heap :+ item // append
    if size > 1
    then promote()

  /** Retrieve the highest priority item.
    *
    * @return
    *   an optional item if `PriorityQueue` is non-empty.
    */
  def max(): Option[A] =
    val result = heap.headOption
    result match
      case Some(_) =>
        if size == 1
        then heap = ArrayBuffer.empty[A]
        else
          swap(Position(1), Position(size))
          heap = heap.init
          demote(Position(1))
        result
      case None => None

  /** Checks if `Queue` is empty.
    *
    * @return
    *   `true` if empty, otherwise `false`.
    */
  def isEmpty: Boolean = heap.isEmpty

  /** Check the number of items contained in the `Queue`.
    *
    * @return
    *   `Zero` if empty, otherwise the items' `count`.
    */
  def size: Int = heap.length

  /** Peeks on the current maximum value item if any.
    *
    * @return
    *   An optional maximum value if `Queue` is non-empty.
    */
  def peek: Option[A] = heap.headOption

  /** Promotes the recently inserted `item` to higher positions in `Queue` if
    * applicable.
    */
  private def promote(): Unit =
    var pos = Position(size)
    while pos.value > 1 do
      val child = heap(pos.toIndex)
      val parent = heap(pos.parent.toIndex)

      if child > parent
      then
        swap(pos, pos.parent)
        pos = pos.parent
      else pos = Position(1) // item is in right order in heap, terminate early
  end promote

  /** Demotes the current head of the `Queue` to lower positions if it's not the
    * maximum value.
    *
    * This method will be called after every call to [[.max()]]
    *
    * @param pos
    *   the position of the item that will be demotted.
    */
  private def demote(pos: Position): Unit =
    val hasLeftChild = heap.isDefinedAt(pos.left.toIndex)
    val hasRightChild = heap.isDefinedAt(pos.right.toIndex)

    (hasLeftChild, hasRightChild) match
      case (true, false) =>
        val child = heap(pos.left.toIndex)
        val parent = heap(pos.toIndex)
        if parent < child
        then swap(pos, pos.left)

      case (true, true) =>
        val parent = heap(pos.toIndex)
        val leftChild = heap(pos.left.toIndex)
        val rightChild = heap(pos.right.toIndex)
        if leftChild >= rightChild && parent < leftChild
        then
          swap(pos, pos.left)
          demote(pos.left)
        else if parent < rightChild
        then
          swap(pos, pos.right)
          demote(pos.right)

      case _ => ()

  end demote

  /** Swaps two items from the internal `heap` in place, given two `Position`
    * values.
    *
    * @param a
    *   first item's position
    * @param b
    *   second items' position
    */
  inline private def swap(a: Position, b: Position): Unit =
    val temp = heap(a.toIndex)
    heap.update(a.toIndex, heap(b.toIndex))
    heap.update(b.toIndex, temp)
  end swap

end MaxPriorityQueue

object MaxPriorityQueue:
  /** Constructs a `Queue` from multiple items.
    * @param items
    *   multiple items to insert in to the `Queue`
    * @return
    *   the constructed `Queue`.
    */
  def apply[A: Ordering](items: A*): MaxPriorityQueue[A] =
    val queue = new MaxPriorityQueue()
    for item <- items do queue.insert(item)
    queue
end MaxPriorityQueue

object PriorityQueueUtil:

  /** A `Position` is the index of an item inside the internal `heap`. This type
    * abstracts the plumbing details of indexing and calculating the parent,
    * left and right positions of an item.
    */
  opaque type Position = Int
  object Position:
    def apply(value: Int): Position = value

  extension (pos: Position)
    inline def value: Int = pos.toInt
    inline def toIndex: Int = pos - 1
    inline def parent = Position(Math.floorDiv(pos, 2)) // Parent's position
    inline def left = Position(pos * 2) // Left child position
    inline def right = Position(pos * 2 + 1) // Right child position

end PriorityQueueUtil
