package section04

sealed trait Either[+E, +A] {
  /* EXERCISE 4-6 */
  // Right 값에 대해 작용하는 버전의 map, flatMap, orElse, map2, Either를 구현하라.
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E,B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => this
    case _ => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(aa => b.map(bb => f(aa,bb)))
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends  Either[Nothing, A]

object Eigher {
  /* EXERCISE 4-7 */
  // Either에 대한 sequence와 traverse를 작성하라.
  // 이 두 함수는 발생한 첫 오류를 돌려주어야 한다 (오류가 발생했다면)
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    ???
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    ???
  }

  /* EXERCISE 4-8 */
  // map2는 오류를 하나만 보고할 수 있다. 두 오류를 모두 보고하게 하려면 어떻게 해야할까?
  // map2를 바구는 것이 좋을까. 아니면 mkPerson 서명을 바꾸는 것이 좋을까?
  // 아니면 이러한 요구사항을 Either보다 더 잘 만족하는 새로운 자료 형식을 만들 수도 있을 것이다.
  // 그러한 자료 형식에 대해 orElse, tarverse, sequence는 다르게 행동할까?


}