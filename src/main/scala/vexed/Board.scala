package vexed

trait Board {
  def getAvailableMoves: Moves
  def isSolved: Boolean
  def isSolveable: Boolean
  def apply(move: Move): Board
  
}
