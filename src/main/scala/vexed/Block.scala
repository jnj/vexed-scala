package vexed

import scalaz.Equal

sealed abstract class Block
case class Moveable(symbol: Char) extends Block
case object Wall extends Block
