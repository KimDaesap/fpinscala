package chapter05

sealed trait Stream[+A] {
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
  // toList 함수를 작성하라
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /* EXERCISE 5-2 */
  // take(n)과 drop(n) 함수를 작성하라
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n -1)
    case _ => this
  }

  /* EXERCISE 5-3 */
  // takeWhile 함수를 작성하라.
  def takeWhile(p: A=>Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  // page 90
  def exists(p: A=>Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  // f의 두번 째 인자 =>B는 함수(비엄격)이기 때문에 평가가 필요하지 않은 상황에서
  // 순회를 중단하게 된다. 엄격한 List의 foldRight와의 차이점이다.
  //   e.g) foldRight(false) ((a,b) => p(a) || b)
  def foldRight[B](z: =>B)(f: (A, =>B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /* EXERCISE 5-4 */
  def forAll(p: A=>Boolean): Boolean = ???

  /* EXERCISE 5-5 */
  // foldRight를 이용해서 takeWhile을 구현하라.
  def takeWhile2(p: A=>Boolean): Stream[A] = ???

  /* EXERCISE 5-6 */
  // foldRight를 이용해서 headOption을 구현하라.
  def headOption2: Option[A] = ???

  /* EXERCISE 5-7 */
  // foldRight를 이용해서 map, filter, append, flatMap을 구현하라.
  def map[B](f: A=>B): Stream[B] = ???

  def filter(p: A=>Boolean): Stream[A] = ???

  def append[B>:A](as: Stream[B]): Stream[B] = ???

  def flatMap[B](f: A=>Stream[B]): Stream[B] = ???

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
}

