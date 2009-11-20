package vexed

class MoveHistory {
  val moves = new Moves
  
  def add(move: Move) = moves.add(move)
  
  def addAll(movesToAdd: Moves) = moves.addAll(movesToAdd)

  def copy = {
    val hist = new MoveHistory
    hist.addAll(moves)
    hist
  }

  override def toString = moves.toString
}
