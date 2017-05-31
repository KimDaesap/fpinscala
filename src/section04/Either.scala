package section04

sealed trait Either[+E, +A] {
  /* EXERCISE 4-6 */
  // Right 값에 대해 작용하는 버전의 map, flatMap, orElse, map2, Either를 구현하라.
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case _ => _
  }

  def flatMap[EE >: E,B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case _ => _
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

