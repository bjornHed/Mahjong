import scala.language.postfixOps
import scala.collection.mutable.ListBuffer


// Class containing the context in which the game was won. Used for counting
// fu and some cases of yaku.
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
        var d2 = new Tile("Bamboo",3)
        var d3 = new Tile("Bamboo",4)
        var d4 = new Tile("Bamboo",5)
        var c2 = new Tile("Character",3)
        var c3 = new Tile("Character",4)
        var c4 = new Tile("Character",5)
        var t5 = new Tile("Dot",6)
        var t6 = new Tile("Dot",7)
        var t7 = new Tile("Dot",8)
          var t8 = new Tile("Dot",9)
    var dragon = new Tile("Dragon",2)
    var drag2 = new Tile("Dragon",3)
    var wind = new Tile("Wind",1)
    var p = new Player
    var dou = ListBuffer[Tile](t2,t2,t2,d2,d2,d2,c2,c2,c2,wind,wind,wind,dragon,dragon)
    var falseHand = ListBuffer[Tile](t0,t0,t0)
    var iit= ListBuffer[Tile](t0,t1,t2,t3,t4,t5,t6,t7,t8,wind,wind,wind,dragon,dragon)
    var hand = ListBuffer[Tile](t1,t1,t1,t2,t2,t2,t3,t3,t3,t4,t4,t4,t5,t5)
    var sevenpairs = ListBuffer[Tile](t1,t1,t2,t2,t3,t3,t4,t4,t5,t5,t6,wind,wind,wind)
    var honr = ListBuffer[Tile](dragon,dragon,dragon,dragon,dragon,dragon,dragon,dragon,dragon,dragon,dragon,dragon,dragon,dragon)
    p.newRound(1,dou)
    var somDrag = ListBuffer[Tile](dragon,dragon,dragon)
  //  println(findShapes(t1,hand))
    println(allShapes(honr))
  //  p.riichi
  //  println(calculateScore(p,true,ListBuffer[Tile](t3)))
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
    yaku += chantaiyao
    yaku += sanshoku_doukou
    yaku += sanshoku_doujun
    yaku += shousangen
    yaku += sanankou

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
    //TODO DOES NOT WORK DISREGARDS KANS
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

    // The hand contains two sets of 3 dragon tiles
    // and a pair of the third dragon tiles.
    def shousangen : Int = {
      var tiles    = hand.getWholeHand
      var triplets = 0
      var pair     = false
      for(i <- 1 to 3) {
        if((tiles.filter(x => x == new Tile("Dragon",i))).size == 3) {
          triplets += 1
        } else if((tiles.filter(x => x == new Tile("Dragon",i))).size == 2) {
          pair = true
        }
      }
      if(triplets == 2 && pair) {
        println("Shousangen            2")
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

    // Three sequences have the same number across the three different suits.
    def sanshoku_doujun : Int = {
      var ya = 2
      if(!hand.isClosed) ya = 1
      var tiles = hand.getWholeHand
      var allGroups = allShapes(tiles)
      var allSuits = List("Bamboo","Character","Dot")
      for(group <- allGroups) {
        var first = group.head
        var otherSuits = allSuits.filter(x => x != first.suit)
        if((otherSuits.map(x => allGroups.contains(copyGroup(x,group))))
              .forall(pred => pred == true)) {
                println("Sanshoku doujun       " + ya)
                return ya
        }

        def copyGroup(s : String, g : ListBuffer[Tile]) : ListBuffer[Tile] = {
          var copyGroup = ListBuffer[Tile]()
          for(i <- 0 until g.length) { copyGroup += new Tile(s,g(i).value) }
          return copyGroup
        }
      }
      return 0
    }

    // Three kans are called for this hand.
    def sankantsu : Int = {
      var tiles = hand.getWholeHand
      if(tiles.size >= 15) {
        println("Sankantsu              2")
        return 2
      }
      return 0
    }

    // The hand includes three groups of triplets (or closed quads)
    // that have been formed without calling any tiles.
    def sanankou : Int = {
      var tiles     = hand.getOpenHand
      var allGroups = allShapes(tiles)
      var triplets  = 0
      for(group <- allGroups) {
        var tripletGroup = group.filter(x => x.value == (group.head).value)
        if(tripletGroup.size == group.size) triplets += 1
      }
      if(triplets >= 3) {
        println("Sanankou              2")
        return 2
      }
      return 0
    }

    // The hand includes three groups of triplets with the same number.
    def sanshoku_doukou : Int = {
      var allGroups = allShapes(hand.getWholeHand)
      var tripletGroups = ListBuffer[Int](0)
      for(group <- allGroups) {
        var filGroup = group.filter(x => x.suit != "Dragon" && x.suit != "Tile")
        var tripletGroup = filGroup.filter(x => x.value == (group.head).value)
        if(tripletGroup.size == group.size) {
          tripletGroups += (group.head).value
        }
      }
      for(i <- 1 to 9) {
        if((tripletGroups.filter(x => x == i)).size >= 3) {
          println("Sanshoku doukou       2")
          return 2
        }
      }
      return 0
    }

    // All tile groups contains atleast one terminal or honor
    def chantaiyao : Int = {
        var tiles = hand.getWholeHand
        var allGroups = allShapes(tiles)
        for (group <- allGroups) {
          var terminals = 0
          for (tile <- group) {
            if(tile.suit == "Dragon" || tile.suit == "Wind" ||
                     tile.value == 9 || tile.value == 1) {
              terminals += 1
            }
            if (terminals < 1) return 0
          }
        }
        if(hand.isClosed) {
          println("Chantaiyao            2")
          return 2
        }
        println("Chantaiyao            1")
        return 1
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

  private[this] def findShapes(tile : Tile, h : ListBuffer[Tile]) : ListBuffer[ListBuffer[Tile]] = {
    var result = ListBuffer[ListBuffer[Tile]]()
    var nmbr   = (h.filter(x => x == tile)).size
    if(nmbr >= 4) { result += ListBuffer[Tile](tile,tile,tile,tile) }
    if(nmbr >= 3) { result += ListBuffer[Tile](tile,tile,tile) }
    if(nmbr >= 2) { result += ListBuffer[Tile](tile,tile) }
    if(tile.suit != "Dragon" && tile.suit != "Wind") {
        var a = tile.value
        if(h.contains(new Tile(tile.suit,(a-1)))
          && h.contains(new Tile(tile.suit,(a+1)))) {
          result += ListBuffer[Tile](tile,
            new Tile(tile.suit,(a-1)),
            new Tile(tile.suit,(a+1)))
        }
        if (h.contains(new Tile(tile.suit,(a+1)))
          && h.contains(new Tile(tile.suit,(a+2)))) {
            result += ListBuffer[Tile](tile,
              new Tile(tile.suit,(a+1)),
              new Tile(tile.suit,(a+2)))
        }
        if(h.contains(new Tile(tile.suit,(a-1)))
            && h.contains(new Tile(tile.suit,(a-2)))) {
              result += ListBuffer[Tile](tile,
                new Tile(tile.suit,(a-1)),
                new Tile(tile.suit,(a-2)))
        }
    }
    return result
  }

  // Does not check Seven Pairs or Thriteen Orphans
  private[this] def isLegitHand(ha : Hand) : Boolean = {
    var ohand = ha.getOpenHand
    var chand = ha.getClosedHand
    var (_,shapesInClosed) = correctShapes(chand,false,ListBuffer[ListBuffer[Tile]]())
    var (result,shapesInOpen) = correctShapes(ohand,false,shapesInClosed)
    return (result && (shapesInOpen.size == 5))
  }

  // Wrapper for easier use of the correctShapes function
  private[this] def allShapes(hand : ListBuffer[Tile]) : ListBuffer[ListBuffer[Tile]] = {
    var (legit,result) = correctShapes(hand,false,ListBuffer[ListBuffer[Tile]]())
    return result
  }

  private[this] def isTripletShape(shape : ListBuffer[Tile]) : Boolean = {
    var first = shape.head
    for(i <- 1 until shape.size) {
      if(shape(i) != first) {
        return false
      }
    }
    return true
  }

  // TODO Bug when containing many Kans.
  private[this] def correctShapes(h : ListBuffer[Tile], pairFound : Boolean, currShapes : ListBuffer[ListBuffer[Tile]])
      : (Boolean, ListBuffer[ListBuffer[Tile]]) = {

    if(h.isEmpty) {
      if(currShapes.size == 5) {
        return (true,currShapes)
      }
      return (false,currShapes)
    }

    var shapes = findShapes(h.head,h)
    for (i <- shapes) {
      if(i.size == 2) {
        if(!pairFound) {
          var newHand = h
          for (item <- i) {
            newHand -= item
          }
          var (legit,nmbrShapes) = correctShapes(newHand,true,currShapes += i)
          if(legit) return (true,nmbrShapes)
        }
      } else {
        var newHand = h
        // Diff seemed not to be working properly, however this should have
        // the same complexity.
        for (item <- i) {
          newHand -= item
        }
        var (legit,nmbrShapes) = correctShapes(newHand,pairFound,currShapes += i)
        if(legit) return (true,nmbrShapes)
      }
    }
    return (false,currShapes)
  }

  private[this] def countFu(hand : Hand) : Int = { return 0 } //undefined

  private[this] def countScore(han : Int, fu : Int) : Int = {
    return 0 //Undefined
  }

  private[this] def countDora(hand : Hand, dora : ListBuffer[Tile]) : Int = {
     val h = hand.getWholeHand
     val nmbr = (h.map(x => dora.count(y => y == x)).sum)
     if(nmbr != 0) {
       println("Dora                  " + nmbr) //Print for testing
     }
     return nmbr
  }
}
