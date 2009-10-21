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
}
