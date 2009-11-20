package vexed

class Moves {
  var moves: List[Move] = List[Move]()

  def add(move: Move) = moves += move

  def addAll(movesToAdd: Moves) = moves ++= movesToAdd.moves

  def size = moves.size
  
  override def toString = moves.toString
}
