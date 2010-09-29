package vexed

import scala.collection.mutable.ListBuffer

class Moves {
  var moves: ListBuffer[Move] = ListBuffer( )

  def add(move: Move) = moves.append(move)

  def addAll(movesToAdd: Moves) = moves ++= movesToAdd.moves

  def size = moves.size
  
  override def toString = moves.toString
}
