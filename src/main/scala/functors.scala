package functors

sealed trait Functor[F[_]]{
  def map[A,B](fa:F[A])(f:A => B):F[B]
}

object Functor{
  def apply[F[_]:Functor]:Functor[F] = implicitly[Functor[F]]

}

trait Contravariant[F[_]]{
  def contramap[A,B](fa:F[A])(f:B => A):F[B]
}

object Predicate {
  type Predicate[A] = A => Boolean
}
import Predicate._

object Contravariant{
  def apply[F[_]:Contravariant] = implicitly[Contravariant[F]]
  implicit object predicate extends Contravariant[Predicate]{
    def contramap[A,B](fa:Predicate[A])(f: B => A):Predicate[B] =  fa compose f
  }
}

