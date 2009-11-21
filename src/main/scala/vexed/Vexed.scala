package vexed

object Vexed {
  def main(argv: Array[String]) = {
    val layout = 
      "#  D  #\n" +
      "#  A  #\n" + 
      "# DC  #\n" + 
      "# AB  #\n" +
      "#ABC C#\n" +
      "#######"
    val board = MapBoard.forLayout(layout)
    val solver = new BfsSolver
    val t0 = java.lang.System.currentTimeMillis
    println(solver.solve(board))
    println((java.lang.System.currentTimeMillis - t0) + " ms")
  }
}
