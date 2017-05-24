package section04

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): Option[B]
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

/* EXERCISE 4-1 */
// Option의 모든 함수들을 구현하라

case class Some[+A] extends Option[A] {
  override def map[B](f: (A) => B): Option[B] = ???

  override def flatMap[B](f: (A) => Option[B]): Option[B] = ???

  override def filter(f: (A) => Boolean): Option[A] = ???

  override def getOrElse[B >: A](default: => B): Option[B] = ???

  override def orElse[B >: A](ob: => Option[B]): Option[B] = ???
}

case class None extends Option[Nothing] {
  override def map[B](f: (Nothing) => B): Option[B] = ???

  override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = ???

  override def filter(f: (Nothing) => Boolean): Option[Nothing] = ???

  override def getOrElse[B >: Nothing](default: => B): Option[B] = ???

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ???
}
