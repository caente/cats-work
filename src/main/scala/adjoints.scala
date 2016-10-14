package adjoints

trait Adjoint[F[_], G[_]] {
  def adjoint[A, B](fa: F[A])(gb: G[B])(f: A => B): B
}

object Adjoint {
  implicit class Syntax[F[_], A](fa: F[A]) {
    def adjoint[B, G[_]](gb: G[B])(f: A => B)(implicit Ad: Adjoint[F, G]): B =
      Ad.adjoint(fa)(gb)(f)
  }
}

