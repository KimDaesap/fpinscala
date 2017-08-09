package chapter06

case class State[S, +A](run: S => (A, S)) {
  import State._

  /* EXERCISE 6-10 */
  // map, map2, flatMap를 일반화하라.
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
}

object State {
  /* EXERCISE 6-10 */
  // unit, sequence 를 일반화하라.
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil    => (acc.reverse, s)
        case h :: t => h.run(s) match { case (a, s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s, sas, List()))
  }

  // page 114.
  // todo: 이게 뭔소리여 -_-;
  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

/* EXERCISE 6-11 */
// 간단한 사탕 판매기를 본뜬 유한상태자동자를 구현하라.
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  import State._

  def update =
    (i: Input) =>
      (s: Machine) =>
        (i, s) match {
          case (_, Machine(_, 0, _)) =>
            s
          case (Coin, Machine(false, _, _)) =>
            s
          case (Turn, Machine(true, _, _)) =>
            s
          case (Coin, Machine(true, candy, coin)) =>
            Machine(locked = false, candy, coin + 1)
          case (Turn, Machine(false, candy, coin)) =>
            Machine(locked = true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)
}
