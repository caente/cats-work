package adjoints

trait Adjoint[F[_], G[_]]{
  def adjoint[A,B](f:A => B)( fb:G[B]):F[A] => B 
}

object Adjoint{
 implicit class Syntax[F[_], A](fa:F[A]){
    def adjoint[B,G[_]](f: A => B)(implicit Ad :Adjoint[F, G],gb:G[B]):B = 
       Ad.adjoint(f)(gb)(fa)
  }
}
