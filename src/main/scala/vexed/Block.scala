package vexed

import scalaz.Equal

sealed abstract class Block {
  implicit val equal = new Equal[Block] {
    def equal(a1: Block, a2: Block) = a1 == a2
  }
}
case class Moveable(symbol: Char) extends Block
case object Wall extends Block
