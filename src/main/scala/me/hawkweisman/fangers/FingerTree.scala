package me.hawkweisman.fangers

import collection.immutable

/**
  * Created by hawk on 2/5/16.
  */
sealed trait FingerTree[V, +A] {

  def prepend[A1 >: A](x: A1): FingerTree[V, A1]

  def :+[A1 >: A](x: A1): FingerTree[V, A1]
    = prepend(x)

}

case class Empty[V, A]
  extends FingerTree[V, A] {

  def prepend[A1 >: A](x: A1): FingerTree[V, A1]
    = Single(x)

}
case class Single[V, A](a: A)
  extends FingerTree[V, A] {

  def prepend[A1 >: A](x: A1): FingerTree[V, A1]
   = Deep(Affix(x), Empty[V, Node[V, A]], Affix(a))

}

case class Deep[V, A]( prefix: Affix[A]
                     , deeper: FingerTree[V, Node[V, A]]
                     , suffix: Affix[A])
  extends FingerTree[V, A] {

  def prepend[A1 >: A](x: A1) : FingerTree[V, A1]
    = ???

}