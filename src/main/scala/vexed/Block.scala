package vexed

object Block {
  val WALL = '#'
}

sealed class Block(val symbol: Char) {
  import Block._

  def isWall = symbol == WALL

  override def toString = symbol.toString

  override def hashCode = symbol.hashCode

  override def equals(obj: Any) = {
    obj match {
      case b: Block => b.symbol == symbol
      case _ => false
    }
  }
}