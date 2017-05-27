package com.perhac.fp.mpilquist

import simulacrum._

import scala.language.{higherKinds, implicitConversions}


@typeclass trait Applicative[F[_]] extends Functor[F] {
  self =>

  def pure[A](a: A): F[A]

  def apply[A, B](fa: F[A])(ff: F[A => B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(fa)(pure(f))

  def apply2[A, B, Z](fa: F[A], fb: F[B])(ff: F[(A, B) => Z]): F[Z] =
    apply(fa)(apply(fb)(map(ff)(f => b => a => f(a, b))))

  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    apply(fa)(map(fb)(b => f(_, b)))

  def map3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => Z): F[Z] =
    apply(fa)(map2(fb, fc)((b, c) => a => f(a, b, c)))

  def tuple2[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  def flip[A, B](fab: F[A => B]): F[A] => F[B] =
    fa => apply(fa)(fab)

  def compose[G[_]](implicit G: Applicative[G]): Applicative[({type fg[x] = F[G[x]]})#fg] =
    new Applicative[({type fg[x] = F[G[x]]})#fg] {

      override def pure[A](a: A): F[G[A]] =
        self.pure(G.pure(a))

      override def apply[A, B](fa: F[G[A]])(ff: F[G[(A) => B]]): F[G[B]] =
        self.apply(fa)(self.map(ff)(G.flip))

    }

}

object Applicative {

  implicit val optionApplicative: Applicative[Option] =
    new Applicative[Option] {
      def pure[A](a: A): Option[A] =
        Some(a)

      def apply[A, B](fa: Option[A])(ff: Option[A => B]): Option[B] =
        (fa, ff) match {
          case (None, _) => None
          case (Some(a), None) => None
          case (Some(a), Some(f)) => Some(f(a))
        }
    }

  implicit val listApplicative: Applicative[List] =
    new Applicative[List] {
      def pure[A](a: A): List[A] =
        List(a)

      def apply[A, B](fa: List[A])(ff: List[A => B]): List[B] =
        for {
          a <- fa
          f <- ff
        } yield f(a)
    }

}
