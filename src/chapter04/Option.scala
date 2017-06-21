package chapter04

/* EXERCISE 4-1 */
// Option의 모든 함수들을 구현하라
sealed trait Option[+A] {

  // map과 flatMap는 함수 인자의 리턴 타입이 값인지 Option 인지가 다르며,
  // map은 리턴을 Some으로 감싸지만 flatMap은 옵션을 벗기는 차이가 있다.
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  /*
   * - '[B >: A]' 는 B 타입은 A 타입과 같거나 상위 타입이어야 한다.
   * - 'default: => B' 형식 주해는 인수의 타입이 B이지만 그 인수가
   *   함수에서 실제로 쓰이기 전까지 평가하지 않는 다는 의미.
   */
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(_) => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if f(v) => this
    case _ => None
  }
}


case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option {
  /* EXERCISE 4-2 */
  def variance(xs: Seq[Double]): Option[Double] = {
    def average(seq: Seq[Double]): Option[Double] = {
      if (seq.isEmpty) None
      else Some(seq.sum / seq.length)
    }

    // - m = Seq의 평균, x = Seq의 각 요소
    // - variance = Seq의 각 요소 x에 대한 math.pow(x - m, 2) 값들의 평균.
    // 1. Seq의 평균 값 m을 구한다.
    // 2. Seq의 요소 x에 대한 match.pow(x - m, 2) 값들의 Seq를 구한다.
    // 3. 2번에서 구한 Seq의 평균 값을 구한다.

    // note: flatMap을 사용하면 average 함수의 리턴 값을 직접 매핑해서
    //       간결하게 표현할 수 있다.
    average(xs).flatMap(m => average(xs.map(x => math.pow(x - m, 2))))
  }

  // page 70.
  // lift(math.abs) -> 인자 Option[A]를 받고 Option[B]를 리턴하는 함수 생성.
  // _ map f -> _는 Option[A]를 가르키고 map 메서드를 사용해서 승급.
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  // page 72.
  // a(lazy)를 평가해서 Option으로 리턴, 예외가 발생하면 None을 리턴.
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None}

  /* EXERCISE 4-3 */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  /* EXERCISE 4-4 */
  // h 가 None이 아닐 때 나머지 목록 t에 대한 map 결과 값을 cons 해야 하기 때문에
  // h.flatMap(v => Option(...)) 가 되고 t에 대해 sequence(t)의 결과 값에 cons를
  // map 하는 함수를 넘기는 방식으로 재귀를 돌면 해결.
  // foldRight 방식으로 마지막 값부터 접는 방식으로 생각해야 이해가 쉽다.
  // a가 Nil인 경우 None을 넘기면 재귀가 중단 되는데 h.flatMap 할 때 Option을 벗긴
  // 값 v에 대해 cons를 하므로 Some(Nil)을 넘겨주면 자연스럽게 List로 연결됨.
  // 좀 더 알기 쉽게 이해할 수 있는 방법을 생각해 봅시다.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap(v => sequence(t).map(v :: _))
    }
  }

  // page 73.
  // 목록 a를 두 번 훑어야 하므로 비효율 적이다.
  // 1. a를 List[Option[Int]]로 변환
  // 2. List[Option[Int]]를 Option[List[Int]]로 변환
  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a map (i => Try(i.toInt)))

  /* EXERCISE 4-5 */
  // 목록 a를 한 번만 훑는 구현을 할 것!
  // 위에서 구현한 map2를 사용하면 다음과 같다.
  //   case h::t => map2(f(h), traverse(t)(f))(_ :: _)
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => f(h).flatMap(v => traverse(t)(f).map(v :: _))
    }
  }

  // sequence를 traverse로 구현해 보라.
  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(v => v)
  }

}
