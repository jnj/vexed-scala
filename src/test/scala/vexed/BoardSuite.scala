import org.scalatest.FunSuite
import vexed.MapBoard

class BoardSuite extends FunSuite {
  test("can initialize board with layout string") {
    val layout = 
      "#   #\n" +
      "# A #\n" +
      "#####"
    val board = new MapBoard(layout)
    assert(layout === board.toString)
  }
  
  test("board is solved if there are no moveable pieces") {
    val layout = 
      "# #\n" + 
      "###"
    val board = new MapBoard(layout)
    assert(board.isSolved)
  }
  
  test("board is solveable if there are no singleton groups") {
    val layout =
      "#B B#\n" +
      "#####"
    val board = new MapBoard(layout)
    assert(board.isSolveable)
  }

  test("board is not solveable if there are singleton groups") {
    val layout =
      "#A  #\n" +
      "#B B#\n" +
      "#####"
    val board = new MapBoard(layout)
    assert(!board.isSolveable)
  }
}
