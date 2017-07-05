package chapter04

sealed trait Either[+E, +A] {
  /* EXERCISE 4-6 */
  // Right 값에 대해 작용하는 버전의 map, flatMap, orElse, map2, Either를 구현하라.
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E,B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] = this match {
    case Right(_) => this
    case _ => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(aa => b.map(bb => f(aa,bb)))
}


case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends  Either[Nothing, A]


object Either {
  /* EXERCISE 4-7 */
  // Either에 대한 sequence와 traverse를 작성하라.
  // 이 두 함수는 발생한 첫 오류를 돌려주어야 한다 (오류가 발생했다면)
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case h :: t => f(h).flatMap(a => traverse(t)(f).map(a :: _))
    }
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    //    es match {
    //      case Nil => Right(Nil)
    //      case h :: t => h.flatMap(v => sequence(t).map(v :: _))
    //    }

    traverse(es)(a => a)
  }

  // page 78, 목록 4.4
  sealed class Name(val value: String)
  sealed class Age(val value: Int)
  case class Person(name: Name, age: Age)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  /* EXERCISE 4-8 */
  // map2는 오류를 하나만 보고할 수 있다. 두 오류를 모두 보고하게 하려면 어떻게 해야할까?
  // map2를 바꾸는 것이 좋을까. 아니면 mkPerson 서명을 바꾸는 것이 좋을까?
  // 아니면 이러한 요구사항을 Either보다 더 잘 만족하는 새로운 자료 형식을 만들 수도 있을 것이다.
  // 그러한 자료 형식에 대해 orElse, tarverse, sequence는 다르게 행동할까?

  // 1. map2를 바꿔보자... 구리다.
  def map2[E, A, B, C](a: Either[E, A], b: Either[E, B])
                      (f: (A, B) => C): Either[List[E], C] = {
    (a, b) match {
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (a1, a2) => Left(List(a1, a2).foldLeft(List[E]()) {
        (acc, x) => x match {
          case Left(e) => e :: acc
          case _ => acc
        }
      })
    }
  }

  // 2. mkPerson의 서명을 바꿔보자...
  def mkPerson_2(name: String, age: Int): Either[List[String], Person] = {
    map2(mkName(name), mkAge(age))(Person(_, _))
  }
}

// 3. Either보다 더 잘 만족하는 새로운 자료형식을 만들어 보자.
// Todo....

