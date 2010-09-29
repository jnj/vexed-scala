package vexed

abstract class Block()
sealed case class Moveable(val symbol: Char) extends Block
sealed case class Wall() extends Block
