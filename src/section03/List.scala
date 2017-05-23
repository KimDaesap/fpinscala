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
		case Cons(x, as) => x + sum(as)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x,as) => x * product(as)
	}

	def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
		case Nil => a2
		case Cons(h,t) => Cons(h, append(t, a2))
	}

	/* 49 page, 목록 3.2
		ex> foldRight(List(1,2,3), 0) ((x,y) => x + y)
		* 우측부터 역순으로 연산.
		Cons(1, Cons(2, Cons(3, Nil)))
		f(1, f(2, f(3, 0))) -> (1 + (2 + (3 + 0))) = 6
	*/
	def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
		case Nil => z
		case Cons(x, as) => f(x, foldRight(as, z)(f))
	}

	def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

	def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

	/* Practice 3-1
		답은 "3". 즉 3번째 case문이 가장 처음 부합되는 조건이기 때문.
	 */
	val x = List(1,2,3,4,5) match {
		case Cons(x, Cons(2, Cons(4, _))) => x
		case Nil => 42
		case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
		case Cons(h, t) => h + List.sum(t)
		case _ => 101
	}

	// Practice 3-2
	def tail[A](l: List[A]): List[A] = l match {
		case Nil => Nil
		case Cons(_, as) => as
	}

	// Practice 3-3
	def setHead[A](a: A, l: List[A]): List[A] = l match {
		case Nil => Cons(a, Nil)
		case Cons(x, as) => Cons(a, as)
	}

	// Practice 3-4
	def drop[A](l: List[A], n: Int): List[A] = {
		if (n > 0) l match {
			case Nil => Nil
			case Cons(x, as) => drop(as, n - 1)
		}
		else l
	}

	// Practice 3-5
	def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
		case Cons(x, as) if f(x) => dropWhile(as, f)
		case _ => l
	}

	// Practice 3-6
	def init[A](l: List[A]): List[A] = l match {
		case Nil => Nil
		case Cons(_, Nil) => Nil
		case Cons(x, as) => Cons(x, init(as))
	}

	/* Practice 3-7
		재귀를 멈추지 않는다. 이유는 재귀를 멈추는 조건이 오로지 값이 Nil일 때만이기 때문.
		평가단축은 목록이 { 1, 2, 3, 4, 5 } 일 경우  1 * ( 2 * ( 3 * (4 * (5 * 1) ) -> 1 * 120
     */

	/* Practice 3-8
		결과는 Cons(1, Cons(2, Cons(3, Nil)))
		결론적으로 List의 생성자가 동작하는 방식과 동일한 방식으로 동작한다.
     */

	/* Practice 3-9
		List(1,2,3)이 입력일 경우 (0 + 1) + 1 + 1 의 형태가 됨.
     */
	def length[A](as: List[A]): Int = {
		List.foldRight(as, 0) ((_, acc) => acc + 1)
	}

	/* Practice 3-10
		ex> foldLeft(List(1,2,3), 0) ((x,y) => x + y)
		- 좌측부터 순차적으로 연산.
		List(1,2,3), 0
		List(2,3), 0 + 1
		List(3), 0 + 1 + 2
		0 + 1 + 2 + 3 = 6
	*/
	def foldLeft[A,B](as: List[A], acc: B) (f: (B, A) => B): B = as match {
		case Nil => acc
		case Cons(h, t) => foldLeft(t, f(acc, h)) (f)
	}

  /* Practice 3-11 */
  def sum3(as: List[Int]): Int =
    foldLeft(as, 0) ((acc, x) => acc + x)

  def product3(as: List[Double]): Double =
    foldLeft(as, 0.0) ((acc, h) => acc * h)

  /* Practice 3-12 */
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]()) ((acc, h) => Cons(h, acc))

  /* Practice 3-13 */
  def foldLeftViaFoldRight[A,B](as: List[A], acc: B) (f: (B,A) => B): B =
    foldLeft(reverse(as), acc) ((acc, h) => f(acc, h))

  /* Practice 3-14 */
  def appendViaFoldRight[A](as: List[A], bs: List[A]): List[A] =
    foldRight(as, bs) (Cons(_,_))

  /* Practice 3-15 */
  def concat[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil:List[A]) (append)

  /* Practice 3-16 */
  def addOne(as: List[Int]) =
    foldRight(as, Nil: List[Int]) ((h, acc) => Cons(h + 1, acc))

  /* Practice 3-17 */
  def doubleToString(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String]) ((h, acc) => Cons(h.toString, acc))

  /* Practice 3-18 */
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B]) ((h, acc) => Cons(f(h), acc))

  /* Practice 3-19 */
  def filter[A](as: List[A]) (f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A]) ((h, acc) => if (f(h)) Cons(h, acc) else acc)

  def removeOdd(as: List[Int]): List[Int] = filter(as) (x => x % 2 == 0)

  /* Practice 3-20 */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  /* Practice 3-21 */
  def filterViaFlatMap[A](as: List[A]) (f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  /* Practice 3-22 */

  /* Practice 3-23 */

  /* Practice 3-24 */

}
