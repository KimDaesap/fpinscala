import chapter06.SimpleRNG

val rng = SimpleRNG(10)
rng.unit(1)

//(0 to 10000).foreach { _ =>
//  val (n, r) = rng.nonNegativeInt(rng)
//  if (n < 0) println(s"rng: $n")
//}

Int.MaxValue + 1

Int.MaxValue
Double.MaxValue

if (Int.MaxValue < Double.MaxValue) println("High") else println("Low")