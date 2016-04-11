package me.hawkweisman.fangers

import collection.immutable

/**
  * Created by hawk on 2/5/16.
  */
sealed trait FingerTree[V, +A] {

  def prepend[A1 >: A](x: A1): FingerTree[V, A1]
  def append[A1 >: A](x: A1): FingerTree[V, A1]

  /** A copy of this `FingerTree` with an element prepended.
    *
    *  @param  x  the prepended element
    *  @tparam A1 the element type of the prepended element
    *  @return    a new collection of type `FingerTree[V, A1]` consisting of `x`
    *             followed by all elements of this `FingerTree`.
    *
    *  Note that :-ending operators are right associative (see example).
    *  A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
    *
    *  Also, the original `FingerTree` is not modified, so you will want to
    *  capture the result.
    */
  @inline final def +:[A1 >: A](x: A1): FingerTree[V, A1] = prepend(x)

  /** A copy of this `FingerTree` with an element appended.
    *
    *  @param  x   the appended element
    *  @tparam A1  the element type of the appended element
    *  @return     a new collection of type `FingerTree[V, A1]` consisting of
    *              all the elements of this `FingerTree` followed by `x`.
    *
    *  Note that :-ending operators are right associative (see example).
    *  A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
    *
    *  Also, the original `FingerTree` is not modified, so you will want to
    *  capture the result.
    */
  @inline final def :+[A1 >: A](x: A1): FingerTree[V, A1] = append(x)

}

case class Empty[V, A]()
extends FingerTree[V, A] {

  def prepend[A1 >: A](x: A1): FingerTree[V, A1]
    = Single(x)

  def append[A1 >: A](x: A1): FingerTree[V, A1]
    = Single(x)

}

case class Single[V, A](a: A)
extends FingerTree[V, A] {

  def prepend[A1 >: A](x: A1): FingerTree[V, A1]
    = Deep( Affix(x)
          , Empty[V, Node[V, A1]]()
          , Affix(a) )

  def append[A1 >: A](x: A1): FingerTree[V, A1]
    = ???

}

case class Deep[V, A]( prefix: Affix[A]
                     , deeper: FingerTree[V, Node[V, A]]
                     , suffix: Affix[A])
extends FingerTree[V, A] {

  def prepend[A1 >: A](x: A1) : FingerTree[V, A1]
    = ???

  def append[A1 >: A](x: A1): FingerTree[V, A1]
    = ???
}
