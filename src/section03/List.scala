package section03

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

	def apply[A](as: A*): List[A] = {
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
	}

	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x,xs) => x * product(xs)
	}

	def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
		case Nil => a2
		case Cons(h,t) => Cons(h, append(t, a2))
	}

	// 49 page, 목록 3-2.
	def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
		case Nil => println(Nil); z
		case Cons(x, xs) => println(s"$x, $xs"); f(x, foldRight(xs, z)(f))
	}

	def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

	def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

}
