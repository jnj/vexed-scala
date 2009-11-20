package vexed

object Vexed {
  def main(argv: Array[String]) = {
    val layout = 
      "#   #\n" + 
      "#A  #\n" + 
      "#B  #\n" +
      "##AB#\n" +
      "#####"
    val board = MapBoard.forLayout(layout)
    val solver = new BfsSolver
    println(solver.solve(board))
  }
}
