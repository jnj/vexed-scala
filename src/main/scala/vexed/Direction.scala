package vexed

object Direction {
  val Left = new Direction(-1, 0)
  val Right = new Direction(1, 0)
  val Down = new Direction(0, 1)
}

class Direction(val columnDelta: Int, val rowDelta: Int) {
  def apply(column: Int, row: Int) = (column + columnDelta, row + rowDelta)
}
