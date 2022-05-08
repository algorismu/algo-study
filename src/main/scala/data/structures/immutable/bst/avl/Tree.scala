package data.structures.immutable.bst.avl

import math.Ordering.Implicits.infixOrderingOps

/** `AVL Tree` is a self balancing binary search tree (BST). This data structure
  * was named after `Adelson-Velsky` and `Landis` and was the `first` invented
  * self-balancing `BST`.
  */
enum Tree[+A]:
  case Empty
  case Leaf(label: A)
  case Node(label: A, left: Tree[A], right: Tree[A])

  /** Counts the number of nodes in `Tree`
    *
    * @return
    *   current number of nodes.
    */
  def size: Int =
    def countNodes(tree: Tree[A], sum: Int): Int =
      tree match
        case Empty   => sum
        case Leaf(_) => 1 + sum
        case Node(_, left, right) =>
          countNodes(left, countNodes(right, sum + 1))
    end countNodes

    countNodes(this, 0)
  end size

  /** Measure height of tree, where height is the longest distance from root
    * `Node` to a `Leaf` node.
    *
    * @return
    *   longest count of edges between `root` node and a `leaf` node.
    */
  def height: Int =
    this match
      case Empty   => -1
      case Leaf(_) => 0
      case Node(_, left, right) =>
        1 + Math.max(left.height, right.height)
  end height

  /** Calculates the balance factor for `Tree`.
    *
    * @return
    *   balance factor between branches of `AVL` tree.
    */
  def balanceFactor: Int =
    this match
      case Empty | Leaf(_) => 0
      case Node(_, left, right) =>
        (left.height + 1) - (right.height + 1)
  end balanceFactor

  /** Checks if `Tree` is balanced.
    *
    * @return
    *   `true` if balanced, otherwise `false`.
    */
  inline def isBalanced: Boolean = balanceFactor > -2 && balanceFactor < 2

  /** Creates a balanced `Tree` from the current tree.
    *
    * @return
    *   A balanced version of the current tree.
    */
  def balanced: Tree[A] =
    if balanceFactor > 1 then rotateRight.pruned
    else if balanceFactor < -1
    then rotateLeft.pruned
    else this
  end balanced

  /** Checks if an `item` exists in `Tree`.
    *
    * @tparam B
    *   type if item to check for its existence.
    * @return
    *   `true` if tree contains the item, otherwise `false`
    */
  def contains[B >: A: Ordering](item: B): Boolean =
    this match
      case Empty       => false
      case Leaf(label) => item == label
      case Node(label, left, right) =>
        if item == label then true
        else if item < label
        then left.contains(item)
        else right.contains(item)
  end contains

  /** Insert an `item` in tree.
    *
    * @return
    *   A balanced `tree` that contains the item.
    */
  def insert[B >: A: Ordering](item: B): Tree[B] =
    if contains(item) then this
    else
      this match
        case Empty => Leaf(item)
        case Leaf(label) =>
          if item < label
          then Node(label, Leaf(item), Empty)
          else Node(label, Empty, Leaf(item))
        case Node(label, left, right) =>
          if item < label
          then Node(label, left.insert(item), right).balanced
          else Node(label, left, right.insert(item)).balanced
  end insert

  private def rotateLeft: Tree[A] =
    this match
      // RL-Rotation
      case Node(x, xL, Node(z, Node(y, yL, yR), zR)) =>
        Node(y, Node(x, xL, yL), Node(z, yR, zR))
      // L-Rotation
      case Node(x, xL, Node(y, yL, yR)) =>
        Node(y, Node(x, xL, yL), yR)
      // Any other pattern, return the same shape
      case _ => this
  end rotateLeft

  private def rotateRight: Tree[A] =
    this match
      // LR-Rotation
      case Node(z, Node(x, xL, Node(y, yL, yR)), zR) =>
        Node(y, Node(x, xL, yL), Node(z, yR, zR))
      // R-Rotation
      case Node(z, Node(y, yL, yR), zR) =>
        Node(y, yL, Node(z, yR, zR))
      // Any other pattern, return the same shape
      case _ => this
  end rotateRight

  private def pruned: Tree[A] =
    this match
      case Node(label, Empty, Empty) => Leaf(label)
      case Node(label, left, right)  => Node(label, left.pruned, right.pruned)
      case _                         => this
  end pruned

end Tree

object AvlTreeUtil:
  import Tree.{Empty, Leaf, Node}
  def flatten[A](tree: Tree[A]): List[A] =
    tree match
      case Empty       => Nil
      case Leaf(label) => List(label)
      case Node(label, left, right) =>
        flatten(left) ::: (label :: flatten(right))
  end flatten

  def debug[A](tree: Tree[A]): String =
    s"Size: ${tree.size}, Height: ${tree.height}, Balance Factor: ${tree.balanceFactor}"
