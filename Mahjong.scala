import scala.language.postfixOps
import scala.collection.mutable.ListBuffer

object Mahjong {
  def main(args: Array[String]): Unit = {

  }

  def runGame = {
    var roundWind  = 1 // East
    var currPlayer = 1
    var dealer     = 1
    var deck = new Deck
    var dora = new DoraWall(deck.getDeck.take(8))
    var player1 = new Player
    var player2 = new Player
    var player3 = new Player
    var player4 = new Player
    // ! Initial wind should be randomized !
    player1.newRound(1,deck.getDeck.take(13))
    player2.newRound(2,deck.getDeck.take(13))
    player3.newRound(3,deck.getDeck.take(13))
    player4.newRound(4,deck.getDeck.take(13))
    // Runs East 1 to 4
    var i = 1
    while(i < 5) {

    }
  }
}
