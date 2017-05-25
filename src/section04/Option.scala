package section04

/* EXERCISE 4-1 */
// Option의 모든 함수들을 구현하라
sealed trait Option[+A] {

  // map과 flatMap는 함수 인자의 리턴 타입이 값인지 Option 인지가 다르다.
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
  def getOrElse[B >: A](default: => B): Option[B] = this match {
    case None => Some(default)
    case Some(_) => this
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

    // 첫 풀이: average(xs).map(m => xs.map(x => math.pow(x - m, 2))).flatMap(average)
    average(xs).flatMap(m => average(xs.map(x => math.pow(x - m, 2))))
  }

}
