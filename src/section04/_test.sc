import section04._

val some = Some(10)
val none = None

some.getOrElse(1)
none.getOrElse(5)

some.flatMap(x => Some(x + 1))

val seq = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
val mean = seq.sum / seq.length


Option.variance(seq)






