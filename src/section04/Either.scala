package section04

sealed trait Either[+E, +A] {
  /* EXERCISE 4-6 */
  // Right 값에 대해 작용하는 버전의 map, flatMap, orElse, map2, Either를 구현하라.
  def map[B](f: A => B): Either[E, B] = ???

  def flatMap[EE >: E,B](f: A => Either[EE, B]): Either[EE, B] = ???

  def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] = ???

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends  Either[Nothing, A]

