package vexed

object Direction {
  val LEFT = new Direction(-1, 0)
  val RIGHT = new Direction(1, 0)
  val DOWN = new Direction(0, 1)
}

class Direction(val columnDelta: Int, val rowDelta: Int) {
  def apply(column: Int, row: Int) = (column + columnDelta, row + rowDelta)
}
