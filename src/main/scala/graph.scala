package common

import cats._
import cats.syntax.eq._
import cats.syntax.functor._
import cats.instances.all._
import cats.data.Xor
import collection.immutable.HashMap

package object graph {
  type Graph[A] = HashMap[A, List[A]]

  object Graph {
    case class Cycle[A](values: List[A])

    def addEdge[A](start: A, end: A, ds: Graph[A]): Graph[A] =
      ds.updated(start, ds.getOrElse(start, Nil) :+ end)

    def disconnected[A](as: List[A]) = HashMap(as.map(_ -> Nil): _*)

    def hasCycle[A: Eq](data: Graph[A]): List[A] = {
      def stoppedAtCycle(a: A, acc: List[A]): List[A] = {
        data.get(a).toList.flatten match {
          case x :: xs if acc.forall(_ =!= x) => stoppedAtCycle(x, x :: acc)
          case x :: xs => acc :+ a
          case Nil => Nil
        }
      }
      data.keys.map { a => stoppedAtCycle(a, List(a)) }.headOption.toList.flatten
    }
  }

  final case class DirectedGraph[A: Eq] private (private val data: Graph[A]) {

    def adjacents(a: A): List[A] = data.get(a).toList.flatten

    def nodes = data.keys.toList

    def last: List[A] = data.filter(_._2.isEmpty).keys.toList

    def filter(f: A => Boolean): DirectedGraph[A] =
      DirectedGraph(
        data.collect { case (a, as) if f(a) => a -> as.filter(f) }
      )

    def map[B: Eq](f: A => B): DirectedGraph[B] =
      DirectedGraph(
        data.map { case (a, as) => f(a) -> as.map(f) }
      )

    def dfs[B](a: A)(z: B)(f: (A, B) => B)(implicit m: Monoid[B]): B =
      adjacents(a) match {
        case Nil => z
        case xs => xs.map { x => dfs(x)(f(x, z))(f) }.foldLeft(m.empty)(m.combine(_, _))
      }

    def connectedWith(f: A => Boolean): List[A] = {
      def dfs(a: A, acc: List[A]): List[A] =
        adjacents(a) match {
          case Nil => acc
          case xs => xs.flatMap(x => dfs(x, x :: acc))
        }
      data.keys.filter(f).flatMap(a => dfs(a, Nil)).toList
    }

    def truncateBy(f: A => Boolean): DirectedGraph[A] = {
      val connected = nodes.filter(f).flatMap(a => connectedWith(_ === a))
      DirectedGraph(
        data.filter {
          case (a, _) => f(a) || connected.exists(_ === a)
        }
      )
    }
    override def toString = data.map {
      case (a, as) => s"$a -> ${as.mkString(", ")}"
    }.mkString("\n")
  }
  object DirectedGraph {

    def apply[A: Eq](elements: List[A])(relation: (A, A) => Boolean): Xor[Graph.Cycle[A], DirectedGraph[A]] = {
      def loop(els: List[A], acc: Graph[A]): Xor[Graph.Cycle[A], Graph[A]] = els match {
        case Nil => Xor.right(acc)
        case x :: xs =>
          val updated = acc.updated(x, elements.filter(s => x =!= s && relation(x, s)))
          val cycle = Graph.hasCycle(updated)
          if (cycle.nonEmpty) Xor.left[Graph.Cycle[A], Graph[A]](Graph.Cycle(cycle))
          else loop(xs, updated)
      }
      loop(elements, HashMap.empty).map(DirectedGraph(_))
    }

  }

  val ls = (1 to 100).toList
  val g = DirectedGraph(ls)((a1, a2) => a1 >= a2 && a1 % a2 == 2)
  val gr = g.getOrElse(throw new Exception())

}
