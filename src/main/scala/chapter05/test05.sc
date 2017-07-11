import chapter05._

val s = Stream(1,2,3,4,5,6,7,8,9,10)
val s2 = s.take(2)
val s3 = s.drop(2)
val s4 = s.takeWhile(_ < 4)

s4.headOption

s.toString
s4.toString

s.take(1).toString

s.takeWhile(_ < 3).toString
s.takeWhile2(_ < 3).toString

s.headOption2
s.drop(2).headOption2

Stream.empty[Int].headOption2

s.map(_ * 2).toString

s.filter(_ % 2 == 0).toString

s.flatMap(a => Stream(a * 2)).toString

Stream.fibs.take(10).toString

Stream.unfold(0)(s => Some((s, s + 1))).take(10).toString

Stream(1,2,3).zipAll(Stream(4,5,6,7)).toString

Stream(1,2,3,4 ,5).tails.toString

