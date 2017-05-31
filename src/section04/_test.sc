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

Option.sequence(List(Some(1), Some(2), Some(3)))

Option.traverse(List("1", "2", "3" , "4", "5"))(x => Option.Try(x.toInt))

val eigherA = Right(10)
val eigherB = Right(5)
val eigherC = Left("I am C")

eigherA.flatMap(x => Right(x * 10))
eigherA.map2(Right("I am B"))((a,b) => Left("I am Right"))
eigherB.map2(eigherA)((a,b) => Left("I am Right"))
eigherB.map2(eigherC)((a,b) => Left("I am Right"))


