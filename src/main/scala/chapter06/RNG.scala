package chapter06

trait RNG {
  def nextInt: (Int, RNG)

  /* EXERCISE 6-1 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = ???

  /* EXERCISE 6-2 */
  def double(rng: RNG): (Double, RNG) = ???

  /* EXERCISE 6-3 */
  def intDouble(rng: RNG): ((Int, Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double, Int), RNG) = ???

  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???

  /* EXERCISE 6-4 */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???

    // page 106~197
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /* EXERCISE 6-5 */
  // 연습문제 6-2의 double을 map을 이용해서 좀 더 우아한 방식으로 구현하라.
  


}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
