package vexed

object Direction {
  val Left = new Direction(-1, 0, "L")
  val Right = new Direction(1, 0, "R")
  val Down = new Direction(0, 1, "D")
}

class Direction(val columnDelta: Int, val rowDelta: Int, val code: String) {
  def apply(column: Int, row: Int) = (column + columnDelta, row + rowDelta)
  override def toString = code
}
