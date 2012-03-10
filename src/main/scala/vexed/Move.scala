package vexed

case class Move(column: Int, row: Int, direction: Direction) {
  def dest = direction(column, row)
  def orig = (column, row)
}
