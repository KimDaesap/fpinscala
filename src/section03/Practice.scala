package section03

object Practice {

	// Practice 3-1.
	// 답은 "3". 즉 3번째 case문이 가장 처음 부합되는 조건이기 때문.
	val x = List(1,2,3,4,5) match {
		case Cons(x, Cons(2, Cons(4, _))) => x
		case Nil => 42
		case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
		case Cons(h, t) => h + List.sum(t)
		case _ => 101
	}

	// Practice 3-2.
	def tail[A](l: List[A]): List[A] = l match {
		case Nil => Nil
		case Cons(_, xs) => xs
	}

	// Practice 3-3.
	def setHead[A](a: A, l: List[A]): List[A] = l match {
		case Nil => Cons(a, Nil)
		case Cons(x, xs) => Cons(a, xs)
	}

	// Practice 3-4.
	def drop[A](l: List[A], n: Int): List[A] = {
		if (n > 0) l match {
			case Nil => Nil
			case Cons(x, xs) => drop(xs, n - 1)
		}
		else l
	}

	// Practice 3-5.
	// 문제 표현이 애매함. 조건이 불일치할 때 까지 요소를 제거 하는 것임.

	/* 이 것은 조건이 일치하는 모든 요소 제거.
	def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
		def go(l: List[A], acc: List[A]): List[A] = l match {
			case Nil => acc
			case Cons(x, xs) => if (f(x)) go(xs, acc) else go(xs, Cons(x, acc))
		}

		go(l, Nil)
	}
	*/

	def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
		case Cons(x, xs) if f(x) => dropWhile(xs, f)
		case _ => l
	}

	// Practice 3-6.
	def init[A](l: List[A]): List[A] = l match {
		case Nil => Nil
		case Cons(_, Nil) => Nil
		case Cons(x, xs) => Cons(x, init(xs))
	}




}
