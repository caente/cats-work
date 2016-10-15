package meetings

import adjoints._
import Adjoint.Syntax
import monoids._
import org.joda.time.DateTime

case class Participant(name: String)

sealed trait Intention[A]
case class Accept[A](a: A) extends Intention[A]
case class Propose[A](a: A) extends Intention[A]
case class Decline[A](a: A) extends Intention[A]
case class Confirm[A](a: A) extends Intention[A]

sealed trait Comunication[+A]
object Comunication {
  def empty[A]: Comunication[A] = NoAction
}
case object NoAction extends Comunication[Nothing]
case class ToSend[A](a: A, to: Participant) extends Comunication[A]
case class Sent[A](a: A, to: Participant, timestamp: DateTime) extends Comunication[A]
case class Received[A](a: A, from: Participant, timestamp: DateTime) extends Comunication[A]

case class Time(t: DateTime)

// Received[Decline[Ics]]
// Received[Accept[Time]]
// Received[Decline[Propose[Time]]]
// ToSend[Decline[Accept[Time]]]
// ToSend[Propose[Time]]

object Intention {

  /*
  Received[Accept[Propose[Time]]] 
  ToSend[Accept[Propose[Time]]] 
  Sent[Accept[Propose[Time]]] 

  ToSend[Propose[Propose[Time]]] 
  Sent[Propose[Propose[Time]]] 

  ToSend[Confirm[Accept[Time]]]
  Sent[Confirm[Accept[Time]]]

  Received[Decline[Propose[Time]]] 
  ToSend[Decline[Propose[Time]]] 
  Sent[Decline[Propose[Time]]] 

  ToSend[Decline[Accept[Time]]] 
  Sent[Decline[Accept[Time]]] 

  ToSend[Decline[Propose[Propose[Time]]]]

  ToSend[Decline[Sent[Accept[Received[Propose[Time]]]]]

  */
  val broker = Participant("broker")
  val a = Participant("a")
  val b = Participant("b")
  val c = Participant("c")

  case class Email(from: Participant, tos: List[Participant] = List(broker), entity: Intention[Time], timestamp: DateTime)

  trait GetValue[A] {
    def value: A
  }
  val now = DateTime.now
  val time = Time(now.plusDays(1))
  val emails: List[Email] = List(
    Email(from = broker, tos = List(a, b), entity = Propose(time), timestamp = now.minusDays(4)),
    Email(from = b, entity = Accept(time), timestamp = now.minusDays(3)),
    Email(from = c, entity = Decline(time), timestamp = now.minusDays(2))
  )

  import cats._
  import cats.syntax.eq._
  import cats.syntax.functor._
  import cats.instances.all._
  import common.graph._

  implicit val eqEmail = Eq.fromUniversalEquals[Email]

  val graphEmails = Graph.create(emails)((e1, e2) => e2.tos.contains(e1.from))

}

