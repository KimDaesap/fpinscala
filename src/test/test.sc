trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  def op(a1: A=>A, a2: A=>A): A=>A = a1 compose a2
  def zero: A=>A = (a: A) => a
}

endoMonoid[Int]
