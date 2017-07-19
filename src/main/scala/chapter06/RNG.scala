package chapter06

trait RNG {
  def nextInt: (Int, RNG)

  /* EXERCISE 6-1 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = nextInt
    (if (n < 0) -n else n, r)
  }

  /* EXERCISE 6-2 */
  // Double의 표현 값의 범위가 Int보다 크기 때문에 +1을 해도 음수가 되지 않는다.
  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), r)
  }

  /* EXERCISE 6-3 */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n1, r1) = rng.nextInt
    val (n2, r2) = double(r1)
    ((n1, n2), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n, d), r) = intDouble(rng)
    ((d, n), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /* EXERCISE 6-4 */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (Nil, rng)
    else {
      val (n, r1) = rng.nextInt
      val (ns, r2) = ints(count -1)(r1)
      (n :: ns, r2)
    }
  }

  // page 196~197
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // map의 사용 예
  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  /* EXERCISE 6-5 */
  // 연습문제 6-2의 double을 map을 이용해서 좀 더 우아한 방식으로 구현하라.
  def double_2(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }

  /* EXERCISE 6-6 */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }
  }

  /* EXERCISE 6-7 */
  // todo: 다시 풀어보기, 어렵다.
  // unit을 안쓰고 람다식을 직접 쓰면 안되는데 이유를 모르겠다 =_=;
  // acc도 List[A]() 말고 Nil[A]를 쓰면 안되는데 왜 그럴까 =_=;
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }

  // todo: 이것도 다시 =_=;
  def ints_2(count: Int)(rng: RNG):  Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  /* EXERCISE 6-8 */

  /* EXERCISE 6-9 */

  /* EXERCISE 6-10 */

  /* EXERCISE 6-11 */




}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
