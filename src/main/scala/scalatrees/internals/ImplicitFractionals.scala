package com.scalatrees.internals

object ImplicitFractionals {
  def apply[T](implicit com: Fractional[T]): Fractional[T] = com

  implicit val booleanFractional: Fractional[Boolean] = new Fractional[Boolean] {
    implicit def bool2int(b:Boolean) = if (b) 1 else 0

    def compare(x: Boolean, y: Boolean): Int = bool2int(x) > bool2int(y)

    def div(x: Boolean, y: Boolean): Boolean = x

    def minus(x: Boolean, y: Boolean): Boolean = x

    def negate(x: Boolean): Boolean = !x

    def plus(x: Boolean, y: Boolean): Boolean = x

    def times(x: Boolean, y: Boolean): Boolean = x

    def toDouble(x: Boolean): Double = x.toDouble

    def toFloat(x: Boolean): Float = x.toFloat

    def toInt(x: Boolean): Int = x

    def toLong(x: Boolean): Long = x.toLong

    def fromInt(x: Int): Boolean = if (x != 0) true else false

    def parseString(x: String): Option[Boolean] = x match {
      case "true" => Some(true)
      case "false" => Some(false)
      case _ => None
    }
  }
}
