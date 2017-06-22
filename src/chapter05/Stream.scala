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

