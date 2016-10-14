package adjoints

import org.scalatest.FunSuite
import Adjoint.Syntax
import monoids._

class AdjointSuite extends FunSuite {

  implicit object listAdjoint extends Adjoint[List, Monoid] {
    def adjoint[A, B](fa: List[A])(mb: Monoid[B])(f: A => B): B = fa match {
      case Nil => mb.zero
      case x :: xs => mb.op(f(x), adjoint(xs)(mb)(f))
    }
  }

  test("adjoint for flatMap") {
    assert(List(1, 2, 3).adjoint(Monoid[List[Int]])(i => List(i)) == List(1, 2, 3).flatMap(i => List(i))) // List(1,2,3)
  }

  test("adjoint for sum") {
    assert(List(1, 2, 3).adjoint(Monoid[Int])(identity) == List(1, 2, 3).sum) // 6
  }

  test("adjoint for length") {
    assert(List(1, 2, 3).adjoint(Monoid[Int])(_ => 1) == List(1, 2, 3).length) // 3
  }

}
