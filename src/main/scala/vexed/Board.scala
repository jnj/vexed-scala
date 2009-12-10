package vexed

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import vexed.Direction._

trait Board {
  def getMoves: List[Move]
  def isSolved: Boolean
  def isSolveable: Boolean
  def applyMove(move: Move): Board
  def moveHistory: MoveHistory
}

object MapBoard {
  def forLayout(layout: String) = new MapBoard(layout, new MoveHistory)
}

class MapBoard(layout: String, val moveHistory: MoveHistory) extends Board {
  private var contents = Map[(Int, Int), Block]()
  private val width = layout.lines.next.size
  private val height = layout.lines.toList.size

  layout.lines.zipWithIndex.foreach { case (line, row) =>
    line.elements.zipWithIndex.foreach { case (char, col) =>
      char match {
        case ' ' => ()
        case '#' => contents += ((col, row) -> Wall())
        case c => contents += ((col, row) -> Moveable(c))
      }
    }
  }

  def getMoves = {
    val positions = occupiedPositions
    val goRight = (p: (Int, Int)) => new Move(p._1, p._2, Right)
    val goLeft = (p: (Int, Int)) => new Move(p._1, p._2, Left)
    val isValidMove = (move: Move) => !blockAt(move.dest)
    (positions.map(goLeft) ++ positions.map(goRight)).filter(isValidMove).toList
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

    symbolCounts.values.forall {_ > 1}
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

  private def blockAt(p: (Int, Int)) = contents.contains(p)

  private def copy = {
    new MapBoard(toString, moveHistory.copy)
  }
  
  private def occupiedPositionsBottomUp = {
    occupiedPositions.toList.sort {_>=_}
  }

  private [vexed] def occupiedPositions = {
    contents.filter {_._2.isInstanceOf[Moveable]}.map {_._1}
  }

  private def doRecordedMove(move: Move) = {
    recordMove(move)
    doMove(move)
    settleAndClear
  }

  private def recordMove(move: Move) = {
    moveHistory.add(move)
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
    val column = for (row <- (p._2 + 1) until height) yield (p._1, row)
    val endPoint = column.findIndexOf { contents.contains(_) }
    if (endPoint == 0) p else column(endPoint - 1)
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

      val matchingNeighbors = neighbors.filter {
        contents.get(_) match {
          case Some(Moveable(c)) if c == block.symbol => true
          case _ => false
        }
      }

      if (matchingNeighbors.size > 0) {
        val q = matchingNeighbors.last
        val group = groups.groupOf(q)
        groups.addToGroup(group, p)
        matchingNeighbors.foreach { groups.addToGroup(group, _) }
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
  
  override def equals(that: Any) = {
    that match {
      case b: MapBoard =>
        contents == b.contents && width == b.width && height == b.height
      case _ => false
    }
  }
  
  override def hashCode = {
    var x = 17
    x = 31 * x + width.hashCode
    x = 31 * x + height.hashCode
    x = 31 * x + contents.hashCode
    x
  }
}
