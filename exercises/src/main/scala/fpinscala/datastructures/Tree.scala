package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => size(left) + size(right) + 1
    }

  def maximum(tree: Tree[Int]): Int = {
    def max(tree: Tree[Int], max_curr: Int): Int =
      tree match {
        case Branch(left, right) => max(left, max_curr).max(max(right, max_curr))
        case Leaf(value) => value.max(max_curr)
      }
    max(tree, Int.MinValue)
  }

  def depth[A](tree: Tree[A]): Int = {
    def maxDepth[B](tree: Tree[B], dep: Int): Int =
      tree match {
        case Branch(left, right) => maxDepth(left, dep + 1).max(maxDepth(right, dep + 1))
        case Leaf(_) => dep + 1
      }
    maxDepth(tree, 0)
  }

  def map[A](tree: Tree[A])(f: A => A): Tree[A] =
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
}