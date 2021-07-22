package fpinscala.laziness

import Stream._
trait Stream[+A] {
  def toList: List[A] = {
    this match {
      case Cons(h, t) => h() :: t().toList
      case Empty => Nil
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    (this, n) match {
      case (_, 0) => Empty
      case (Cons(h, t), n) => Cons(h, () => t().take(n - 1))
      case _ => Empty
    }

  def drop(n: Int): Stream[A] =
    (n, this) match {
      case (0, _) => this
      case (n, Cons(_, t)) => t().drop(n - 1)
      case _ => Empty
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Cons(() => a, () => b) else Empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(Option.empty[A])((a, _) => a match {
      case h => Some(h)
    })

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Cons(() => f(a), () => b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Cons(() => a, () => b) else b)

  // TO DO
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a))

  def append[B >: A](elem: B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => (a, b) match {
      case (aa, Empty) => Cons(() => aa, () => Stream(elem))
      case (aa, bb) => Cons(() => aa, () => bb)
    })

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
  def fib(n1: Int = 0, n2: Int = 1): Stream[Int] = Stream.cons(n1, fib(n2, n1 + n2))
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}