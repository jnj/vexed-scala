package vexed

object Vexed {

  def time(action: => Unit): Long = {
    val start = java.lang.System.currentTimeMillis
    action
    val stop = java.lang.System.currentTimeMillis
    stop - start
  }

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
    val time_diff = time { println(solver.solve(board)) }
    
    println(time_diff + " ms")
  }
}
