package vexed

sealed abstract class Direction(val columnDelta: Int, val rowDelta: Int, val code: String) {
  def apply(column: Int, row: Int) = (column + columnDelta, row + rowDelta)
  override def toString = code
}

case object Left extends Direction(-1, 0, "L")
case object Right extends Direction(1, 0, "R")
case object Down extends Direction(0, 1, "D")

