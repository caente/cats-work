package common

import cats._
import cats.syntax.eq._
import cats.syntax.functor._
import cats.instances.all._
import cats.data.Xor
import collection.immutable.HashMap

package object graph {
  type Graph[A] = HashMap[A, List[A]]
  final case class DirectedGraph[A: Eq] private (private val data: Graph[A]) {

    def adjacents(a: A): List[A] = data.get(a).toList.flatten

    def last: List[A] = data.filter(_._2.isEmpty).keys.toList

    def filter(f: A => Boolean): DirectedGraph[A] =
      DirectedGraph(
        data.collect { case (a, as) if f(a) => a -> as.filter(f) }
      )

    def map[B: Eq](f: A => B): DirectedGraph[B] =
      DirectedGraph(
        data.map { case (a, as) => f(a) -> as.map(f) }
      )

    def fromNode(f: A => Boolean): DirectedGraph[A] = {
      def isNeeded(x: A): Boolean =
        data.exists { case (a, as) => f(a) && as.exists(_ === x) }
      DirectedGraph(
        data.collect {
          case (a, as) if (f(a)) => a -> as
          case (a, _) if isNeeded(a) => a -> Nil
        }
      )
    }

    override def toString = data.map {
      case (a, as) => s"$a -> ${as.mkString(", ")}"
    }.mkString("\n")
  }
  object DirectedGraph {

    case class Cycle[A](values: List[A])
    private def hasCycle[A: Eq](data: Graph[A]): List[A] = {
      def stoppedAtCycle(a: A, acc: List[A]): List[A] = {
        data.get(a).toList.flatten match {
          case x :: xs if acc.forall(_ =!= x) => stoppedAtCycle(x, x :: acc)
          case x :: xs => acc :+ a
          case Nil => Nil
        }
      }
      data.keys.map { a => stoppedAtCycle(a, List(a)) }.headOption.toList.flatten
    }

    def apply[A: Eq](elements: List[A])(relation: (A, A) => Boolean): Xor[Cycle[A], DirectedGraph[A]] = {
      def loop(els: List[A], acc: Graph[A]): Xor[Cycle[A], Graph[A]] = els match {
        case Nil => Xor.right(acc)
        case x :: xs =>
          val updated = acc.updated(x, elements.filter(s => x =!= s && relation(x, s)))
          val cycle = hasCycle(updated)
          if (cycle.nonEmpty) Xor.left[Cycle[A], Graph[A]](Cycle(cycle))
          else loop(xs, updated)
      }
      loop(elements, HashMap.empty).map(DirectedGraph(_))
    }

  }

  val ls = (1 to 100).toList
  val g = DirectedGraph(ls)((a1, a2) => a1 >= a2 && a1 % a2 == 2)
  val gr = g.getOrElse(throw new Exception())

}
