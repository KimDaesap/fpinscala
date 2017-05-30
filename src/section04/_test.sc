import section04._

val some = Some(10)
val none = None

some.getOrElse(1)
none.getOrElse(5)

some.flatMap(x => Some(x + 1))

val seq = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
val mean = seq.sum / seq.length

Option.variance(Seq())
Option.variance(seq)
Option.lift(math.abs)(Some(-1))

val a = Some(1)
val b = None
def f(a: Int, b: Int): Int = {
  a + b
}

a.flatMap(aa => b.map(bb => f(aa, bb)))

Option.map2(Some(1), Some(2))(f)


def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  a match {
    case h:: t => h.flatMap(v => sequence(t).map(v :: _))
    case Nil => println("Nil"); Some(Nil)
  }
}

sequence(List(Some(1), Some(2), Some(3)))

//Some(1).flatMap(v => Some(v))