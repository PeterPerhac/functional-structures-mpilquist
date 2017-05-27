package com.perhac.fp.mpilquist

import simulacrum._

import scala.language.implicitConversions
import scala.language.higherKinds

@typeclass trait Functor[F[_]] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def compose[G[_]](implicit G: Functor[G]): Functor[Lambda[X => F[G[X]]]] =
    new Functor[Lambda[X => F[G[X]]]] {
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = self.map(fga)(ga => G.map(ga)(f))
    }
}


trait FunctorLaws {
  def identity[F[_], A](fa: F[A])(implicit F: Functor[F]) = F.map(fa)(a => a) == fa

  def composition[F[_], A, B, C](fa: F[A], f: A => B, g: B => C)(implicit F: Functor[F]) =
    F.map(F.map(fa)(f))(g) == F.map(fa)(f andThen g)
}

object Functor {
  implicit val optionFunctor = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
  }
  implicit val listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }
  implicit def function1Functor[X]: Functor[X => ?] = new Functor[X => ?] {
    override def map[A, B](fa: (X) => A)(f: (A) => B): (X) => B = fa andThen f
  }
  implicit def mapFunctor[K]: Functor[Map[K, ?]] =
    new Functor[Map[K, ?]] {
      override def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] =
        fa.map { case (k, a) => (k, f(a)) }
    }
  // the below is same as above, just doesn't require kind prjector compiler plugin
  // implicit def mapFunctor[K]: Functor[({ type L[v] = Map[K, v] })#L] =
  //   new Functor[({ type L[v] = Map[K, v] })#L] {
  //     override def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] =
  //       fa.map { case (k, a) => (k, f(a)) }
  //   }
}
