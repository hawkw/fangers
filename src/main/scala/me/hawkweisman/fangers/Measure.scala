package me.hawkweisman.fangers



/**
  * Created by hawk on 2/5/16.
  */
trait Measure[-A, M] {
  import Measure.Zip

  def zero: M
  def apply(it: A): M

  def |+| (a: M, b: M): M
  def |+| (a: M, b: M, c: M): M = |+|(|+|(a, b), c)

  final def zip[A1 <: A, N](m: Measure[A1, N]): Measure[A1, (M, N)]
    = new Zip(this, m)

}

object Measure {

  object Empty extends Measure[Any, Unit] {
    override val zero = ()
    override def apply(it: Any): Unit = ()

    def |+|(a: Unit, b: Unit): Unit = ()

  }

  private final class Zip[A, M, N](m1: Measure[A, M], m2: Measure[A, N])
    extends Measure[A, (M, N)] {

    def zero: (M, N) = (m1.zero, m2.zero)
    def apply(c: A): (M, N) = (m1.apply(c), m2.apply(c))

    def |+|(a: (M, N), b: (M, N)): (M, N)
      = (m1.|+|(a._1, b._1), m2.|+|(a._2, b._2))

    override def |+|(a: (M, N), b: (M, N), c: (M, N)): (M, N)
      = (m1.|+|(a._1, b._1, c._1), m2.|+|(a._2, b._2, c._2))
  }
}