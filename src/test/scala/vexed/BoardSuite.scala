package vexed

import org.scalatest.FunSuite
import scala.collection.immutable.HashSet

class BoardSuite extends FunSuite {
  test("can initialize board with layout string") {
    val layout = 
      "#   #\n" +
      "# A #\n" +
      "#####"
    val board = MapBoard(layout)
    assert(layout === board.toString)
  }
  
  test("board is solved if there are no moveable pieces") {
    val layout = 
      "# #\n" + 
      "###"
    val board = MapBoard(layout)
    assert(board.isSolved)
  }
  
  test("board is solveable if there are no singleton groups") {
    val layout =
      "#B B#\n" +
      "#####"
    val board = MapBoard(layout)
    assert(board.isSolveable)
  }

  test("board is not solveable if there are singleton groups") {
    val layout =
      "#A  #\n" +
      "#B B#\n" +
      "#####"
    val board = MapBoard(layout)
    assert(!board.isSolveable)
  }
  
  test("occupiedPositions returns only positions with moveables") {
    val layout =
      "#A #\n" +
      "####"
    val board = MapBoard(layout)
    val expected = new HashSet + ((1,0))
    assertEqualIgnoreOrder(expected, board.occupiedPositions.keys)
  }

  test("getMoves returns all possible moves") {
    val layout =
      "# A #\n" + 
      "# B #\n" + 
      "#####"
    val board = MapBoard(layout)
    val expected = List(Move(2, 0, Right), Move(2, 0, Left),
                        Move(2, 1, Right), Move(2, 1, Left))
    assertEqualIgnoreOrder(expected, board.getMoves)
  }

  test("applyMove moves piece") {
    val layout =
      "#A #\n" +
      "####"
    val board = MapBoard(layout)
    val expectedLayout = 
      "# A#\n" + 
      "####"
    val expectedBoard = MapBoard(expectedLayout)
    assert(expectedBoard === board.applyMove(Move(1, 0, Right)))
  }

  test("applyMove causes piece to fall") {
    val layout =
      "#A #\n" +
      "#  #\n" +
      "####"
    val board = MapBoard(layout)
    val expectedLayout =
      "#  #\n" +
      "# A#\n" +
      "####"
    val expectedBoard = MapBoard(expectedLayout)
    assert(expectedBoard === board.applyMove(Move(1, 0, Right)))
  }

  test("applyMove causes piece to fall until it hits other block") {
    val layout =
      "#A #\n" +
      "## #\n" +
      "# B#\n" +
      "####"
    val board = MapBoard(layout)
    val expectedLayout =
      "#  #\n" +
      "##A#\n" +
      "# B#\n" +
      "####"
    val expectedBoard = MapBoard(expectedLayout)
    assert(expectedBoard === board.applyMove(Move(1, 0, Right)))
  }

  test("groups vanish") {
    val layout =
      "#A #\n" +
      "## #\n" +
      "# A#\n" +
      "####"
    val board = MapBoard(layout)
    val expectedLayout =
      "#  #\n" +
      "## #\n" +
      "#  #\n" +
      "####"
    val expectedBoard = MapBoard(expectedLayout)
    assert(expectedBoard === board.applyMove(Move(1, 0, Right)))
  }

  test("at most one group is cleared before allowing the blocks to fall again") {
    val layout =
        "#  D  #\n" +
        "#  B  #\n" +
        "# DC  #\n" +
        "# AB  #\n" +
        "#ABC C#\n" +
        "########"
    val board = MapBoard(layout).applyMove(Move(3, 2, Right))
    assert(board.isSolved, "board should be solved: \n" + board)
  }

  test("proper sequence of moves will solve board") {
    val layout =
      "#  D  #\n" +
      "#  A  #\n" +
      "# DC  #\n" +
      "# AB  #\n" +
      "#ABC C#\n" +
      "#######"
    var board = MapBoard(layout)
    board = board.applyMove(Move(3, 1, Left))

    board = board.applyMove(Move(2, 1, Left))
    board = board.applyMove(Move(3, 1, Left))
    board = board.applyMove(Move(3, 2, Right))
    assert(board.isSolved, "board not solved: \n" + board)
  }

  test("corner groups will vanish") {
    val layout =
      "# A  #\n" +
      "# #  #\n" +
      "# A  #\n" +
      "#A#  #\n" +
      "######"
    val board = MapBoard(layout)
    val expectedLayout =
      "#    #\n" +
      "# #  #\n" +
      "#    #\n" +
      "# #  #\n" +
      "######"
    val expectedBoard = MapBoard(expectedLayout)
    assert(expectedBoard === board.applyMove(Move(2, 0, Left)))
  }

  private def assertEqualIgnoreOrder[T](a: Iterable[T], b: Iterable[T]) {
    val s = new HashSet[T] ++ a
    val t = new HashSet[T] ++ b
    assert(s === t)    
  }
}
