package vexed

trait Board {
  def getAvailableMoves: List[Move]
  def isSolved = getAvailableMoves.size == 0
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
