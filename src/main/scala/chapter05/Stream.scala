package chapter05

sealed trait Stream[+A] {
  import Stream.cons

  // 디버깅용 출력 함수
  def print: String = this match {
    case Cons(h, t) => h().toString + ", " + t().print
    case _ => "End"
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /* EXERCISE 5-1 */
  // toList 함수를 작성하라.
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /* EXERCISE 5-2 */
  // take(n)과 drop(n) 함수를 작성하라.
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n -1)
    case _ => this
  }

  /* EXERCISE 5-3 */
  // takeWhile 함수를 작성하라.
  def takeWhile(p: A=>Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  // page 90-1
  def exists(p: A=>Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  // page 90-2
  // f의 두번 째 인자 =>B는 함수(비엄격)이기 때문에 평가가 필요하지 않은 상황에서
  // 순회를 중단하게 된다. 엄격한 List의 foldRight와의 차이점이다.
  //   e.g) foldRight(false) ((a,b) => p(a) || b)
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /* EXERCISE 5-4 */
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  /* EXERCISE 5-5 */
  // foldRight를 이용해서 takeWhile을 구현하라.
  def takeWhile2(p: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A])((a, acc) => if (p(a)) cons(a, acc) else Empty)

  /* EXERCISE 5-6 */
  // foldRight를 이용해서 headOption을 구현하라.
  def headOption2: Option[A] =
    this.foldRight(None: Option[A])((a, acc) => Some(a))

  /* EXERCISE 5-7 */
  // foldRight를 이용해서 map, filter, append, flatMap을 구현하라.
  def map[B](f: A => B): Stream[B] =
    this.foldRight(Stream.empty[B])((a, acc) => cons(f(a), acc))

  def filter(p: A=>Boolean): Stream[A] =
    this.foldRight(Stream.empty[A])((a, acc) => if (p(a)) cons(a, acc) else acc)

  // 왜 인자 b의 타입 B는 반공변으로 선언해야 동작하는가?!
  //   참고) http://www.bench87.com/content/32 (프로그래밍 스칼라 10.1.1~2)
  def append[B >: A](b: Stream[B]): Stream[B] =
    this.foldRight(b)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(Stream.empty[B]) ((a, acc) => f(a).append(acc))
}


case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Stream {
  // Cons 생성용 유틸리티 함수. 중복 평가를 피하기 위해 lazy val을 사용한다.
  def cons[A](hd: =>A, tl: =>Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  // 빈 Stream은 Empty로 표현가능하지만 타입추론이 필요한 경우에는 이 함수를 사용한다.
  //   e.g) Stream.empty[A]
  def empty[A]: Stream[A] = Empty

  // _*: Seq 타입의 와일드 카드, List의 tail 등의 타입을 표현.
  // applySeq(), unapplySeq() 등의 Extractor와 연관이 있다.
  // 참고 :
  //   http://docs.scala-lang.org/ko/tutorials/tour/extractor-objects.html
  //   https://blog.outsider.ne.kr/502
  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  /* EXERCISE 5-8 */
  // 무한 Stream을 돌려주는 constrant를 구현하라.
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /* EXERCISE 5-9 */
  // n에서 시작해서 n + 1, n + 2,  등으로 이어지는 무한 Stream을 생성하는 함수를 작성하라.
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /* EXERCISE 5-10 */
  // 무한 피보나치 수 0, 1, 1, 2, 3, 5, 8, ...으로 이루어진 무한 Stream 생성 함수 fibs를 구현하라.
  // standard library를 사용한다면 다음 같은 코드가 된다.
  //   lazy val fibs: Stream[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }
  def fibs: Stream[Int] = {
    def loop(prevPrev: Int, prev: Int): Stream[Int] =
      cons(prevPrev, loop(prev, prevPrev + prev))

    loop(0, 1)
  }

  /* EXERCISE 5-11 */
  // 좀 더 일반화된 스트림 구축 함수 unfold를 작성하라.
  // A 타입만으로도 동작은 가능하다 굳이 S타입이 필요한 이유는 뭘까...?
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Cons(() => a, () => unfold(s)(f))
    case _ => Empty
  }

  /* EXERCISE 5-12 */
  // unfold를 이용해서 fibs, from, constant, ones를 작성하라.

  /* EXERCISE 5-13 */
  // unfold를 이용해서 map, take, takeWhile, zipWith, zipAll을 구현하라.
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???

  /* EXERCISE 5-14 */
  // 앞에서 작성한 함수들을 이용해서 startsWith를 구현하라.
  def startsWith[A](s: Stream[A]): Boolean = ???

  /* EXERCISE 5-15 */
  // unfold를 이용해서 tails를 구현하라.
  def tails: Stream[Stream[A]] ???

  /* EXERCISE 5-16 */
  // tails를 일반화한 scanRight 함수를 작성하라.
  //   e.g) Stream(1,2,3).scanRight(0)(_ + _).toList
  //         -> List[Int] = List(6, 5, 3, 0)

}

