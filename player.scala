import scala.language.postfixOps
import scala.collection.mutable.ListBuffer

class Player {
  private var wind = 0
  private var hand = new Hand
  private var points = 25000
  private var ric = false //Keeps track if the player is in riichi

  // ! SHhould perhaps have a guard
  def riichi = {
    points -= 1000
    ric = true
  }

  def getRiichi = ric
  def getHand = hand
  def getWind = wind
  def getPoints = points
  def newRound(newWind : Int, tiles : ListBuffer[Tile]) = {
    wind = newWind
    hand.initHand(tiles)
  }
}

class Hand {
  // Open is the hand you can discard from, concealed from opponents.
  private var open   = ListBuffer[Tile]()
  // Closed are locked tiles from stealing. Visible to opponents.
  private var closed = ListBuffer[Tile]()

  // Checks if the hand is closed.
  def isClosed : Boolean = {
    if(closed.size == 0) {
      return true
    }
    return false
  }

  def initHand(tiles: ListBuffer[Tile]) = {
     open = tiles
     this.sort
  }

  /* Discards a given tile from the list
     of open tiles. If no such tile exists
     no operation is performed */
  def discard(tile: Tile) = {
    open -= tile
    this.sort
  }

  /* Insertionsort algorithm since the hand
     will be small (at most a size of 18) and
     will be almost sorted in all but the
     initial case. */
  def sort = {
    for(i <- 1 until open.length) {
      val x = open(i)
      var j : Int = i - 1
      while(j >= 0 && open(j) > x) {
        open.update((j+1),open(j))
        j -= 1
      }
      open.update((j+1),x)
    }
  }

  def addToHand(tile: Tile) = { open += tile }
  override def toString : String = open.toString + ", " + closed.toString

  /* Definition for the 'Kan'-operation.
     Removes the kan from the open tiles and
     add them to the closed set.
     Returns true if the operation was successful,
     otherwise false for when the hand contains
     less than 3 of a given tile. */
  def kan(tile: Tile) : Boolean = {
    if(open.count(x => x == tile) >= 3) {
      open   = open.filter(x => x != tile)
      closed += (tile,tile,tile,tile)
      return true
    }
    return false
  }

  def getWholeHand : ListBuffer[Tile] = { return (open ++ closed) }
  def getOpenHand : ListBuffer[Tile] = { return open }
  def getClosedHand : ListBuffer[Tile] = { return closed }
}
