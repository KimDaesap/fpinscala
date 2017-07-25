package chapter03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Leaf가 없는 Branch는 어떻게 표현해야할까?
// e.g) Branch(Leaf(1), Nothing)

object Tree {

  /* EXERCISE 3-25 */
  def size[A](tr: Tree[A]): Int = tr match {
    case Leaf(_)      => 1
    case Branch(l, r) => size(l) + size(r)
  }

  /* EXERCISE 3-26 */
  def maximum(tr: Tree[Int]): Int = tr match {
    case Leaf(v)      => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  /* EXERCISE 3-27 */
  def depth[A](tr: Tree[A]): Int = tr match {
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + depth(l) max depth(r)
  }

  /* EXERCISE 3-28 */
  def map[A, B](tr: Tree[A])(f: A => B): Tree[B] = {
    tr match {
      case Leaf(v)      => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  /* EXERCISE 3-29 */
  // 생각중...
  def fold[A, B](tr: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    tr match {
      case Leaf(v)      => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }
}
