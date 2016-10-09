package adjoints

import org.scalatest.FunSuite
import Adjoint.Syntax
import monoids._

class AdjointSuite extends FunSuite {

  implicit object listAdjoint extends Adjoint[List, Monoid] {
    def adjoint[A,B](f: A => B)( mb:Monoid[B]): List[A] => B = {
      case Nil =>  mb.zero
      case x :: xs => mb.op(f(x), adjoint(f)(mb)(xs) )
    }
  }

  test ("adjoint for flatMap"){
    assert( List(1,2,3).adjoint(i => List(i)) == List(1,2,3).flatMap(i => List(i)) ) // List(1,2,3)
  }

  test ("adjoint for sum"){
   assert( List(1,2,3).adjoint(identity) == List(1,2,3).sum ) // 6
  }

  test ("adjoint for length") {
    assert( List(1,2,3).adjoint(_ => 1) == List(1,2,3).length ) // 3
  }

  
}
