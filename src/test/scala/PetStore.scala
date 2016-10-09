package adjoints


import org.scalatest.FunSuite
import Adjoint.Syntax
import monoids._


case class Address(address: String)

sealed trait PetStatus[A]
case class Sold[A](address: Address) extends PetStatus[A]
case class InShelter[A]() extends PetStatus[A]


sealed trait PetOwnership[A]
case class Customer[A](name: String, address: Address) extends PetOwnership[A]
case class NoOwner[A]() extends PetOwnership[A]

case class Dog(name: String)
case class Cat(name: String)


class PetStoreTests extends FunSuite{

  implicit object petAdjoint extends Adjoint[PetStatus, PetOwnership]{
    def adjoint[A,B](f: A => B)(gb:PetOwnership[B]): PetStatus[A] => B = {
     case Sold(address) => ???
     case InShelter() => ???
    }
  }
  test("pet store"){}
}
