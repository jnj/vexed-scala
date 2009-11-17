package vexed

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import vexed.Direction._

trait Board {
  def getMoves: List[Move]
  def isSolved
  def isSolveable: Boolean
  def applyMove(move: Move): Board 
}

class MapBoard(layout: String) {
  private var contents = Map[(Int, Int), Block]()
  private val width = layout.lines.next.size
  private val height = layout.lines.toList.size

  layout.lines.zipWithIndex.foreach { case (line, row) =>
    line.elements.zipWithIndex.foreach { case (char, col) =>
      char match {
        case ' ' => ()
        case '#' => contents = contents + ((col, row) -> Wall())
        case c => contents = contents + ((col, row) -> Moveable(c))
      }
    }
  }

  def getMoves = {
    val moves = new ListBuffer[Move]

    occupiedPositions.foreach { case (col, row) =>
      if (!contents.contains(Right(col, row))) {
        moves += new Move(col, row, Right)
      }

      if (!contents.contains(Left(col, row))) {
        moves += new Move(col, row, Left)
      }
    }

    moves
  }

  def isSolveable = {
    val symbolCounts = new HashMap[Char, Int]()

    contents.values.foreach {
      case Wall() => ()
      case Moveable(c) => {
        val count = symbolCounts.get(c) match {
          case None => 1
          case Some(n) => n + 1
        }
        symbolCounts += (c -> count)
      }
    }

    symbolCounts.values.forall { i => i > 1 }
  }
  
  def isSolved = contents.values.forall {
    case Wall() => true
    case _ => false
  }

  def applyMove(move: Move) = {
    val newBoard = copy
    newBoard.doRecordedMove(move)
    newBoard
  }

  private def copy = {
    new MapBoard(toString)
  }
  
  private def occupiedPositionsBottomUp = {
    occupiedPositions.toList.sort {_>=_}
  }

  private def occupiedPositions = {
    contents.filter {_._2.isInstanceOf[Moveable]}.map {_._1}
  }

  private def doRecordedMove(move: Move) = {
    recordMove(move)
    doMove(move)
    settleAndClear
  }

  private def recordMove(move: Move) = {
    //moveHistory.add(move)
  }
  
  private def doMove(m: Move) = {
    move(m.orig, m.dest)
  }

  private def move(src: (Int, Int), dst: (Int, Int)) = {
    contents += (dst -> contents.get(src).get)
    contents -= src
  }

  private def settleAndClear = {
    do settle while (clearGroups)
  }

  private def settle = {
    occupiedPositionsBottomUp.foreach { p =>
      val landingPos = findLandingPosition(p)
      if (landingPos != p) {
        move(p, landingPos)
      }
    }
  }

  private def findLandingPosition(p: (Int, Int)) = {
    (1, 2)
  }

  private def clearGroups = {
    val groups = findGroups
    contents = contents -- groups
    groups.size > 0
  }

  private def findGroups = {
    val groups = new Groups

    occupiedPositionsBottomUp.foreach { p => 
      val block = contents(p).asInstanceOf[Moveable]
      val neighbors = List(Right(p._1, p._2), Down(p._1, p._2))

      val matchingNeighbors = neighbors.filter { q => 
        contents.get(q) match {
          case Moveable(c) if c == block.symbol => true
          case _ => false
        }
      }

      if (matchingNeighbors.size > 0) {
        val q = matchingNeighbors(0)
        groups.addToGroup(groups.groupOf(q), p)
      } else {
        groups.addToNewGroup(p)
      }
    }
    
    groups.nonSingletons
  }

  override def toString = {
    val buf = new StringBuilder()
    
    for (row <- 0 until height; col <- 0 until width) {
      val c = contents.get(col, row) match {
        case Some(Wall()) => '#'
        case Some(Moveable(c)) => c 
        case None => ' '
      }
      
      buf.append(c)
      
      if (col == width - 1 && row < height - 1)
        buf.append("\n")
    }      
    
    buf.toString
  }
}
