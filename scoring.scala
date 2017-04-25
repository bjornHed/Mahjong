import scala.language.postfixOps
import scala.collection.mutable.ListBuffer

class Context(lastTile : Boolean, lastDiscard : Boolean, deadWall : Boolean) {

}

object Scoring {
  // DEFINITION FOR TESTING
  def main(args: Array[String]): Unit = {
    var t0 = new Tile("Dot",1)
    var t1 = new Tile("Dot",2)
        var t2 = new Tile("Dot",3)
        var t3 = new Tile("Dot",4)
        var t4 = new Tile("Dot",5)
        var t5 = new Tile("Dot",6)
        var t6 = new Tile("Dot",7)
        var t7 = new Tile("Dot",8)
          var t8 = new Tile("Dot",9)
    var dragon = new Tile("Dragon",2)
    var wind = new Tile("Wind",1)
    var p = new Player
    var iit= ListBuffer[Tile](t0,t1,t2,t3,t4,t5,t6,t7,t8,wind,wind,wind,dragon,dragon)
    var hand = ListBuffer[Tile](t1,t1,t1,t2,t2,t2,t3,t3,t3,t4,t4,t4,t5,t5)
    var sevenpairs = ListBuffer[Tile](t1,t1,t2,t2,t3,t3,t4,t4,t5,t5,t6,wind,wind,wind)
    var honr = ListBuffer[Tile](dragon,dragon,dragon,dragon,dragon,dragon,dragon,dragon,dragon,dragon,dragon,dragon,dragon,dragon)
    p.newRound(1,hand)
    println(isLegitHand(p.getHand))
    p.riichi
    println(calculateScore(p,true,ListBuffer[Tile](t3)))
  }
  /* Returns the respective points to be added/subtracted
     in case of a draw */
  def draw (h1 : Hand, h2 : Hand, h3 : Hand, h4 : Hand) : (Int,Int,Int,Int) = {
    return (0,0,0,0) //Undefined
  }

  def isTenpai(hand : Hand) : Boolean = {
    return false //undefined
  }

  def calculateScore(
    player : Player,
    isDealer : Boolean,
    dora : ListBuffer[Tile]) : (String,Int) = {

    var multiplier = 1.0
    if (isDealer) { multiplier = 1.5 }
    var hand = player.getHand

    var yaku = countYaku(player)
    var fu   = 0
    if (yaku == 0) {
      // With zero Yaku you cannot gain points even from Dora.
      return ("No yaku", 0)
    }
    yaku += countDora(hand, dora) //yaku is now Han
    if (yaku < 5) {
      fu = countFu(hand)
      return ("Below Mangan", (multiplier*(countScore(yaku,fu))).toInt)
    } else {
      yaku match {
        case 5 => return ("Mangan", ((8000.0*multiplier).toInt))
        case it if 6 to 7 contains it =>
            return ("Haneman", ((12000.0*multiplier).toInt))
        case it if 8 to 10 contains it =>
            return ("Baiman", ((16000.0*multiplier).toInt))
        case it if 11 to 12 contains it =>
            return ("Sanbaiman", ((24000.0*multiplier).toInt))
        case _ => return ("Kazoe-yakuman", ((32000.0*multiplier).toInt))
      }
    }
  }

  private[this] def countYaku(player : Player) : Int = {
    var hand = player.getHand
    var yaku = 0
    yaku += riichi
    yaku += tanyao
    yaku += yakuhai
    yaku += chiitoitsu
    yaku += toitoi
    yaku += honroutou
    yaku += iitsu
    yaku += honitsu

    def riichi : Int = {
      if(player.getRiichi) {
        println("Riichi                1")
        return 1
      }
      return 0
    }
    // All simples
    def tanyao : Int = {
      var tiles = hand.getWholeHand
      var filTiles = tiles.filter(x => (x.value != 1 && x.value != 9))
      filTiles = filTiles.filter(x => (x.suit != "Dragon" && x.suit != "Wind"))
      if(tiles == filTiles) {
        println("Tanyao                1") //Print for testng
        return 1
      }
      return 0
    }

    // All triplets
    def toitoi : Int = {
      var triplets = 0
      var pairs    = 0
      var tiles    = hand.getWholeHand

      var checked = ListBuffer[Tile]()
      // Checks for same tile
      for (a <- 0 until 13 if !(checked.contains(tiles(a)))) {
        (tiles.filter(x => x == tiles(a))).size match {
          case 2 => if (pairs > 0) {return 0} else {pairs += 1}
          case it if 3 to 4 contains it => triplets += 1
          case _ => return 0
        }
        checked += tiles(a)
      }
      println("Toitoi                2")
      return 2
    }

    // Only terminals and honours
    def honroutou : Int = {
      val tiles = hand.getWholeHand
      val filterd =
        tiles.filter(x => x.suit == "Dragon" || x.suit == "Wind"
                        || x.value == 1 || x.value == 9)
      if(filterd.length == tiles.length) {
        println("Honroutou             2")
        return 2
      }
      return 0
    }

    // One suit mixed with honours
    def honitsu : Int = {
      var tiles = hand.getWholeHand
      var allSuits = List("Dot","Bamboo","Character")
      allSuits.map(s => if(tiles == (tiles.filter(x =>
        x.suit == "Dragon" || x.suit == "Wind" || x.suit == s))) {
          if(hand.isClosed) {
            println("Honitsu               3")
            return 3
          }
          println("Honitsu               2")
          return 2
        })
      return 0
    }

    // 1 to 9 of one suit.
    def iitsu : Int = {
        val tiles = hand.getWholeHand
        val allSuits = List("Dot","Bamboo","Character")
        var result = 0
        allSuits.foreach(su => result += helper(su))

          def helper(s : String) : Int = {
            val nmbrs = 1 to 9 toList
            val series = (tiles.filter(x => x.suit == s)).distinct
            val f = ListBuffer(nmbrs.map(x => new Tile(s,x))).flatten
            if(f == series) {
              if(hand.isClosed) {
                println("Iitsu                 2")
                return 2
              }
              println("Iitsu                 1")
              return 1
            }
            return 0
          }
      return result
    }

    // Seven pairs
    def chiitoitsu : Int = {
      if(hand.isClosed) {
        var curr = hand.getWholeHand
        for ( a <- List(1,3,5,7,9,11,13) ) {
          if(!(curr(a) == curr(a-1))) {
            return 0
          }
        }
        println("Chiitoitsu            2")
        return 2
      }
      return 0
    }
    //Dragon, round wind or seatwind
    // ! TODO: For now the round wind is always East !
    def yakuhai : Int = {
      var y = 0
      var tiles = hand.getWholeHand
      var wind = player.getWind
      if((tiles.filter(x => (x == new Tile("Dragon",1)))).size >= 3) {
        println("Green Dragon          1") //Print for testng
        y += 1
      }
      if((tiles.filter(x => (x == new Tile("Dragon",2)))).size >= 3) {
        println("Red Dragon            1") //Print for testng
        y += 1
      }
      if((tiles.filter(x => (x == new Tile("Dragon",3)))).size >= 3) {
        println("White Dragon          1") //Print for testng
        y += 1
      }
      var east = 0
      if((tiles.filter(x => (x == new Tile("Wind",wind)))).size >= 3) {
        y += 1
        wind match {
          case 1 => east += 1
          case 2 => println("South                 1")
          case 3 => println("West                  1")
          case 4 => println("North                 1")
        }
      }
      if((tiles.filter(x => (x == new Tile("Wind",1)))).size >= 3) {
        y = east + 1
        println("East                  " + y)
      }
      return y
    }

    return yaku
  }

  // TODO
  // ! Disregards seven pairs and thirteen orphans !
  def isLegitHand(hand : Hand) : Boolean = {
    var triplets = 0
    var pairs    = 0
    var tiles    = hand.getWholeHand

    var checked = ListBuffer[Tile]()
    // Checks for same tile
    for (a <- 0 until (tiles.size -1) if !(checked.contains(tiles(a)))) {
      (tiles.filter(x => x == tiles(a))).size match {
        case 2 => if (pairs > 0) {return false} else {pairs += 1}
        case it if 3 to 4 contains it => triplets += 1
        case _ => return false
      }
      checked += tiles(a)
    }
    tiles = hand.getWholeHand
    if(triplets == 4 && pairs == 1) {
      return true
    }
    return false //Undefined
  }

  private[this] def countFu(hand : Hand) : Int = { return 0 } //undefined

  private[this] def countScore(han : Int, fu : Int) : Int = {
    return 0 //Undefined
  }

  private[this] def countDora(hand : Hand, dora : ListBuffer[Tile]) : Int = {
     val h = hand.getWholeHand
     val nmbr = (h.map(x => dora.count(y => y == x)).sum)
     if(nmbr != 0) {
       println("Dora                  " + nmbr) //Print for testing, remove later
     }
     return nmbr
  }
}
