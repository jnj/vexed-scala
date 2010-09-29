package vexed

import scala.collection.immutable.Queue

trait Solver {
  def solve(board: Board): Solution
}

class BfsSolver {
  def solve(rootBoard: Board): Solution = {
    var seenBoards = Set[Board]()
    var queue = Queue(rootBoard)

    while (!queue.isEmpty) {
      val (board, q) = queue.dequeue
      queue = q

      if (board.isSolved)
        return new Solution(board.moveHistory, seenBoards.size)

      if (!seenBoards.contains(board)) {
        seenBoards += board
        queue = explore(board, queue)
      }
    }
      
    throw new java.lang.IllegalArgumentException("unsolveable board")
  }
  
  private def explore(board: Board, queue: Queue[Board]) = {
    var q = queue
    board.getMoves.foreach { m => q = q.enqueue(board.applyMove(m)) }
    q
  }
}
