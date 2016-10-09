package meetings


import org.scalatest.FunSuite
import adjoints._
import Adjoint.Syntax
import monoids._
import org.joda.time.DateTime


case class Participant(name: String)

sealed trait Intention[A]
case class Accept[A](a:A) extends Intention[A]
case class Propose[A](a:A) extends Intention[A]
case class Decline[A](a:A) extends Intention[A]
case class Confirm[A](a:A) extends Intention[A]

sealed trait Comunication[A]
case class Send[A](a:A, to:Participant) extends Comunication[A]
case class Received[A](a:A, from: Participant, timestamp: DateTime) extends Comunication[A]

case class Time(t:DateTime)
case class Ics(t: DateTime)

// Received[Decline[Ics]]
// Received[Accept[Time]]
// Received[Decline[Propose[Time]]]
// Send[Decline[Accept[Time]]]
// Send[Propose[Time]]
// Send[Propose[Ics]]
// Send[Decline[Ics]]
object Intention {
  def accept(ls:List[Comunication[Intention[Time]]]):List[Send[Accept[Propose[Time]]]] = ???
  def propose(ls:List[Comunication[Intention[Time]]]):List[Send[Propose[Propose[Time]]]] = ???
  def suggestTimes(ls:List[Comunication[Intention[Time]]]):List[Send[Propose[Time]]] = ???
  def confirm(ls:List[Comunication[Intention[Time]]]):List[Send[Confirm[Accept[Time]]]] = ???
  def declineProposed(ls:List[Comunication[Intention[Time]]]):List[Send[Decline[Propose[Time]]]] = ???
  def declineAccepted(ls:List[Comunication[Intention[Time]]]):List[Send[Decline[Accept[Time]]]] = ???
//  implicit object intentionAdjoint extends Adjoint[Received, Send]{
//    def adjoint[A,B](f: A => B)(fb:Send[B]):AllReceived[A] => B = ???
//  }
}

