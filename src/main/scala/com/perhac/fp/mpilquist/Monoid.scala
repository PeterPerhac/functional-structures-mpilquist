package com.perhac.fp.mpilquist

import simulacrum._

import scala.language.{higherKinds, implicitConversions}

@typeclass trait Monoid[T] {
  self =>

    def zero: T

    def op(t1:T, t2:T):T
}

object Monoid{

   implicit object IntAdditionMonoid extends Monoid[Int] {
     override def zero: Int = 0
     override def op(i1: Int, i2: Int): Int = i1 + i2
   }

   implicit object IntMultiplicationMonoid extends Monoid[Int] {
     override def zero: Int = 1
     override def op(i1: Int, i2: Int): Int = i1 * i2
   }

   implicit object BooleanAndMonoid extends Monoid[Boolean] {
     override def zero: Boolean = true
     override def op(b1: Boolean, b2: Boolean): Boolean = b1 && b2
   }

   implicit object BooleanOrMonoid extends Monoid[Boolean] {
     override def zero: Boolean = false
     override def op(b1: Boolean, b2: Boolean): Boolean = b1 || b2
   }

   def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]]{
     override def zero: Option[A] = Option.empty[A]
     override def op(o1: Option[A], o2: Option[A]): Option[A] = o1 orElse o2
   }

   def endoMonoid[A]: Monoid[A=>A] = new Monoid[A=>A]{
     override def zero: A=>A = identity[A]
     override def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
   }

}

