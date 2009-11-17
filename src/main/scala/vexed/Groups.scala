package vexed

class Groups {
  var idsToGroups = Map[Int, List[(Int, Int)]]().withDefault {_ => List()}
  var reversedMap = Map[(Int, Int), Int]()
  var groupNum = 0
  
  def nextGroupNum = {
    groupNum += 1
    groupNum
  }

  def addToNewGroup(p: (Int, Int)) = addToGroup(nextGroupNum, p)
  
  def addToGroup(group: Int, p: (Int, Int)) = {
    idsToGroups += (group -> (idsToGroups(group) + p))
    reversedMap += (p -> group)
  }
  
  def apply(i: Int) = idsToGroups(i)

  def groupOf(p: (Int, Int)) = reversedMap(p)

  // compiler gets confused w/o return type here
  def nonSingletons: List[(Int, Int)] = {
    idsToGroups.filter {_._2.size > 1}.map {_._2}.toList.flatten
  }
}
