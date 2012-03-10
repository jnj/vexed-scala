package vexed

class Groups {
  var idsToGroups = Map.empty[Int, List[(Int, Int)]].withDefault {_ => List.empty}
  var reversedMap = Map.empty[(Int, Int), Int]
  var groupNum = 0
  
  def nextGroupNum = {
    groupNum += 1
    groupNum
  }

  def addToNewGroup(p: (Int, Int)) {
    addToGroup(nextGroupNum, p)
  }
  
  def addToGroup(group: Int, p: (Int, Int)) {
    idsToGroups += (group -> (p :: idsToGroups(group)))
    reversedMap += (p -> group)
  }
  
  def apply(i: Int) = idsToGroups(i)

  def groupOf(p: (Int, Int)) = reversedMap(p)

  def firstNonSingletonGroup = nonSingletons.headOption

  def nonSingletons = idsToGroups.filter {_._2.size > 1}.map {_._2}.toList

  override def toString =
    "Groups{idsToGroups=" + idsToGroups + ", reversedMap=" + reversedMap + "}"
}
