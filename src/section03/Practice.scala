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

	// Practice 3-7.
	// 재귀를 멈추지 않는다. 이유는 재귀를 멈추는 조건이 오로지 값이 Nil일 때만이기 때문.
	// 평가단축은 목록이 { 1, 2, 3, 4, 5 } 일 경우  1 * ( 2 * ( 3 * (4 * (5 * 1) ) -> 1 * 120

	// Practice 3-8.
	// 결과는 Cons(1, Cons(2, Cons(3, Nil)))
	// 결론적으로 List의 생성자가 동작하는 방식과 동일한 방식으로 동작한다.

	// Practice 3-9.
	// ?_? acc가 어떻게 동작하는지 잘 모르겠음. 연구중.
	def length[A](as: List[A]): Int = {
		List.foldRight(as, 0) ((_, acc) => acc + 1)
	}

	// Practice 3-10.
	def foldLeft[A,B](as: List[A], z: B) (f: (B, A) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => f(foldLeft(xs, x)(f), z)

	}


	// Practice 3-11.

	// Practice 3-12.

	// Practice 3-13.

	// Practice 3-14.

	// Practice 3-15.




}
