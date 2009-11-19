package vexed

import org.scalatest.FunSuite
import scala.collection.immutable.HashSet

import vexed.Direction._

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
  
  test("occupiedPositions returns only positions with moveables") {
    val layout =
      "#A #\n" +
      "####"
    val board = new MapBoard(layout)
    val expected = new HashSet + ((1,0))
    assertEqualDiscountingOrder(expected, board.occupiedPositions)
  }

  test("getMoves returns all possible moves") {
    val layout =
      "# A #\n" + 
      "# B #\n" + 
      "#####"
    val board = new MapBoard(layout)
    val expected = List(new Move(2, 0, Right), new Move(2, 0, Left),
                        new Move(2, 1, Right), new Move(2, 1, Left))
    assertEqualDiscountingOrder(expected, board.getMoves)
  }

  test("applyMove moves piece") {
    val layout =
      "#A #\n" +
      "####"
    val board = new MapBoard(layout)
    val expectedLayout = 
      "# A#\n" + 
      "####"
    val expectedBoard = new MapBoard(expectedLayout)
    assert(expectedBoard === board.applyMove(new Move(1, 0, Right)))
  }

  test("applyMove causes piece to fall") {
    val layout = 
      "#A #\n" +
      "#  #\n" + 
      "####"
    val board = new MapBoard(layout)
    val expectedLayout = 
      "#  #\n" +
      "# A#\n" + 
      "####"
    val expectedBoard = new MapBoard(expectedLayout)
    assert(expectedBoard === board.applyMove(new Move(1, 0, Right)))
  }

  test("applyMove causes piece to fall until it hits other block") {
    val layout = 
      "#A #\n" +
      "## #\n" +
      "# B#\n" +
      "####"
    val board = new MapBoard(layout)
    val expectedLayout = 
      "#  #\n" +
      "##A#\n" +
      "# B#\n" +
      "####"
    val expectedBoard = new MapBoard(expectedLayout)
    assert(expectedBoard === board.applyMove(new Move(1, 0, Right)))
  }

  test("groups vanish") {
    val layout = 
      "#A #\n" +
      "## #\n" +
      "# A#\n" +
      "####"
    val board = new MapBoard(layout)
    val expectedLayout = 
      "#  #\n" +
      "## #\n" +
      "#  #\n" +
      "####"
    val expectedBoard = new MapBoard(expectedLayout)
    assert(expectedBoard === board.applyMove(new Move(1, 0, Right)))
  }

  private def assertEqualDiscountingOrder[T](a: Iterable[T], b: Iterable[T]) {
    val s = new HashSet[T] ++ a
    val t = new HashSet[T] ++ b
    assert(s === t)    
  }
}
