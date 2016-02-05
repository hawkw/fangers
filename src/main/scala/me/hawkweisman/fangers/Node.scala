package me.hawkweisman.fangers

import org.scalactic._

/**
  * Created by hawk on 2/5/16.
  */
sealed abstract class Node[V, A] {
  def toList: List[A]
}
case class Branch2[V, A](a1: A, a2: A)
  extends Node[V, A] {

  def toList: List[A]
    = List(a1, a2)
}
case class Branch3[V, A](a1: A, a2: A, a3: A)
  extends Node[V, A] {

  def toList: List[A]
    = List(a1, a2)
}

object Node
extends Requirements {

  def apply[V, A](xs: List[A]): Node[V, A]
    = { require(xs.length == 2 || xs.length == 3)
        xs match {
          case x1 :: x2 :: Nil       => Branch2[V, A](x1, x2)
          case x1 :: x2 :: x3 :: Nil => Branch3[V, A](x1, x2, x3)
          case _ => throw new IllegalArgumentException // this never happens
        }
      }

}
