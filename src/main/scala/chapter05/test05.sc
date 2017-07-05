import chapter05._

val s = Stream(1,2,3,4,5,6,7,8,9,10)
val s2 = s.take(2)
val s3 = s.drop(2)
val s4 = s.takeWhile(_ < 4)

s4.headOption

s.print
s4.print

s.take(1).print

s.takeWhile(_ < 3).print
s.takeWhile2(_ < 3).print

s.headOption2
s.drop(2).headOption2

Stream.empty[Int].headOption2

s.map(_ * 2).print

s.filter(_ % 2 == 0).print

s.flatMap(a => Stream(a * 2)).print

Stream.fibs.take(10).print

Stream.unfold(0)(s => Some((s, s + 1))).take(10).print


