package product

trait Product[A,B, C] {
    def fg:C => (A,B)
    val p1:((A,B)) => A = _._1
    val p2:((A,B)) => B = _._2
    def f:C => A = p1 compose fg
    def g :C => B = p2 compose fg
  }

object P {

val t = new Product[Int, String, (Int, Double, String)]{
    def fg = (x: (Int, Double, String)) => (x._1, x._3)
  }
}
