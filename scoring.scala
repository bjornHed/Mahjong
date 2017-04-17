import scala.language.postfixOps
import scala.collection.mutable.ListBuffer

object Scoring {
  // DEFINITION FOR TESTING
  def main(args: Array[String]): Unit = {
    var t1 = new Tile("Dot",2)
    var t2 = new Tile("Dragon",2)
    var t3 = new Tile("Wind",1)
    var p = new Player
    p.newRound(1,ListBuffer[Tile](t1,t1,t1,t1,t1,t1,t3,t3,t3,t1,t2,t2,t2,t2))
    println(calculateScore(p,false,ListBuffer[Tile](t1)))
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

    def riichi : Int = {
      if(player.getRiichi) {
        println("Riichi                1")
        return 1
      }
      return 0
    }
    // Tanyao = All simples
    def tanyao : Int = {
      var tiles = hand.getWholeHand
      tiles = tiles.filter(x => (x.value != 1 && x.value != 9))
      tiles = tiles.filter(x => (x.suit != "Dragon" && x.suit != "Wind"))
      if(tiles.size == 14) {
        println("Tanyao                1") //Print for testng
        return 1
      }
      return 0
    }

    //Dragon, round wind or seatwind
    // ! TODO: Round wind not yet implemented. !
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
      if((tiles.filter(x => (x == new Tile("Wind",wind)))).size >= 3) {
        y += 1
        wind match {
          case 1 => println("East                  1")
          case 2 => println("South                 1")
          case 3 => println("West                  1")
          case 4 => println("North                 1")
        }
      }
      return y
    }

    return yaku
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
