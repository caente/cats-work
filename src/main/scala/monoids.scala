package monoids

sealed trait Monoid[A] {
  def zero: A
  def op(a1: A, a2: A): A
}

object Monoid {
  def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]
  implicit object sum extends Monoid[Int] {
    def zero = 0
    def op(a1: Int, a2: Int) = a1 + a2
  }
  implicit object mult extends Monoid[Double] {
    def zero = 1
    def op(a1: Double, a2: Double) = a1 * a2
  }
  implicit def list[A] = new Monoid[List[A]] {
    def zero = List.empty[A]
    def op(l1: List[A], l2: List[A]) = l1 ++ l2
  }

}
