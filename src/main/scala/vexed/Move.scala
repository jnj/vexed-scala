package vexed

class Move(val column: Int, val row: Int, val direction: Direction) {

  override def equals(obj: Any) = {
    obj match {
      case move: Move => move.column == column && move.row == row && move.direction == direction
      case _ => false
    }
  }

  def orig = (column, row)
    
  def dest = direction(column, row)

  override def toString = "[(" + column + "," + row + ") " + direction + "]"

  override def hashCode = {
    var x = 17
    x = 31 * x + column.hashCode
    x = 31 * x + row.hashCode
    x = 31 * x + direction.hashCode
    x
  }
}
