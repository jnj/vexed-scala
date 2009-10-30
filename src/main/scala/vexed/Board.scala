package vexed

import scala.collection.mutable.HashMap
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

  layout.lines.zipWithIndex.foreach { t => 
    val (line, row) = t
    line.elements.zipWithIndex.foreach { u =>
      val (char,col) = u
      char match {
        case ' ' => ()
        case '#' => contents = contents + ((col, row) -> Wall())
        case c => contents = contents + ((col, row) -> Moveable(c))
      }
    }
  }

  def getMoves = {
    val moves = new scala.collection.mutable.ListBuffer[Move]

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
    contents.values.foreach { block =>
      block match {
        case Wall() => ()
        case Moveable(c) => {
          val count = symbolCounts.get(c) match {
            case None => 1
            case Some(n) => n + 1
          }
          symbolCounts += (c -> count)
        }
      }
    }
    symbolCounts.values.forall { i => i > 1 }
  }
  
  def isSolved = contents.values.forall { block =>
    block match {
      case Wall() => true
      case _ => false
    }
  }
  
  def occupiedPositions = {
    val filtered = contents.filter { e => 
      e match {
        case (_, Moveable(c)) => true
        case _ => false
      }
    }
    
    filtered.map { e => e._1 }
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
