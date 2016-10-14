package common

import cats._
import cats.syntax.eq._

package object graph {
  type Graph[A] = List[Node[A]]

  trait InnerMap[F[_], G[_]] {
    def innerMap[A, B](fa: F[G[A]])(f: A => B): F[G[B]]
  }

  implicit object graphInnerMap extends InnerMap[List, Node] {
    def innerMap[A, B](fa: List[Node[A]])(f: A => B): List[Node[B]] =
      fa.map(_.map(f))
  }

  implicit class InnerMapSyntax[F[_], G[_], A](v: F[G[A]]) {
    def innerMap[B](f: A => B)(implicit I: InnerMap[F, G]): F[G[B]] = I.innerMap(v)(f)
  }

  case class Node[A](value: A, children: Graph[A]) {
    def map[B](f: A => B): Node[B] = Node(
      value = f(value),
      children = children.map(_.map(f))
    )

    def expand[B](f: A => List[B]): Graph[B] = f(value).map {
      b =>
        Node(
          value = b,
          children = children.flatMap(_.expand(f))
        )
    }
  }

  object Graph {

    def create[A: Eq](elements: List[A])(relation: (A, A) => Boolean): Graph[A] = {
      elements.sortWith(relation) match {
        case Nil => Nil
        case x :: xs =>
          val (adjacents, orphans) = xs.partition(e => x =!= e && relation(x, e))
          Node(x, create(adjacents)(relation)) :: create(orphans)(relation)
      }
    }
  }

}