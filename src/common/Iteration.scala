package common

sealed trait Iteration[+R]

case class Yield[+R](result: R, next: () => Iteration[R]) extends Iteration[R]

case object Done extends Iteration[Nothing]
