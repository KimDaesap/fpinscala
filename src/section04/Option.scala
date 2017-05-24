package section04

/* EXERCISE 4-1 */
// Option의 모든 함수들을 구현하라

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = ???

  def flatMap[B](f: A => Option[B]): Option[B] = ???

  def getOrElse[B >: A](default: => B): Option[B] = ???

  def orElse[B >: A](ob: => Option[B]): Option[B] = ???

  def filter(f: A => Boolean): Option[A] = ???

}

case class Some[+A] extends Option[A]
case class None extends Option[Nothing]
