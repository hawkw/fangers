package me.hawkweisman.fangers

/**
  * Created by hawk on 2/5/16.
  */
sealed trait Affix[A]

case class One[A](a: A)
  extends Affix[A]
case class Two[A](a1: A, a2: A)
  extends Affix[A]
case class Three[A](a1: A, a2: A, a3: A)
  extends Affix[A]
case class Four[A](a1: A, a2: A, a3: A, a4: A)
  extends Affix[A]

object Affix {
  // TODO: is using method dispatch the fastest way to do this?
  def apply[A](x: A): Affix[A] = One(x)
  def apply[A](x1: A, x2: A): Affix[A] = Two(x1, x2)
  def apply[A](x1: A, x2: A, x3: A): Affix[A] = Three(x1, x2, x3)
  def apply[A](x1: A, x2: A, x3: A, x4: A): Affix[A] = Four(x1, x2, x3, x4)
}