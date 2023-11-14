package sutd.compiler

import sutd.compiler.Functor.*
import sutd.compiler.Applicative.*

object Monad {

    trait Monad[F[_]] extends Applicative[F] {
        def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
        def pure[A](v: A): F[A]
        def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
            bind(ff)((f: A => B) => bind(fa)((a: A) => pure(f(a))))
    }

    given listMonad: Monad[List] = new Monad[List] {
        def pure[A](v: A): List[A] = List(v)
        def bind[A, B](fa: List[A])(f: A => List[B]): List[B] =
            fa.flatMap(f)
    }

    trait MonadError[F[_], E] extends Monad[F] with ApplicativeError[F, E] {
        override def pure[A](v: A): F[A]
        override def raiseError[A](e: E): F[A]
        override def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

    }


    def traverse[A,B,F[_]](f: A => F[B], l:List[A])(using m:Monad[F]):F[List[B]] = l match {
        case Nil => m.pure(Nil)
        case (a::as) => m.bind(f(a))(
            b => m.bind(traverse(f,as))( 
                bs => m.pure(b::bs))
        )
    }
    
    def foldM[A,B,F[_]](f: (B,A) => F[B])(acc:B)(l:List[A])(using m:Monad[F]):F[B] = l match {
        case Nil => m.pure(acc)
        case (a::as) => m.bind(f(acc,a))(
            acc1 => m.bind(foldM(f)(acc1)(as))(
                acc2 => m.pure(acc2)
            )
        )
    }


}
