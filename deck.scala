import scala.language.postfixOps
import scala.collection.mutable.ListBuffer

class DoraWall(doras : ListBuffer[Tile]) {
  var open = ListBuffer[Tile]()
  var hidden = ListBuffer[Tile]()
  open += doras.head
  hidden ++ doras.tail

  def flip = {
    // After the first 4 dora the rest is ura-dora.
    if(open.size < 4) {
      open += hidden.head
      hidden = hidden.tail
    }
  }
}

class Tile(s: String, v: Int) extends Ordered[Tile] {
  def value = v
  def suit = s

  def compare (that: Tile) : Int = {
    if (this.suit == that.suit) {
      return (this.value - that.value)
    } else {
      this.suit match {
        case "Character" => return -1
        case "Dot" => if (that.suit == "Character") { 1 } else { -1 }
        case "Bamboo" => if (that.suit != "Dragon" && that.suit != "Wind") {
                            1
                         } else {
                           -1
                         }
        case "Wind" => if (that.suit != "Dragon") { 1 } else { -1 }
        case _      => 1
      }
    }
  }

  override def equals(that: Any): Boolean = {
    that match {
      case that: Tile => that.suit == s && that.value == v
      case _ => false
    }
  }

  override def toString: String = "(" + s + ", " + v + ")"
}

class Deck {
  private var deck = ((ListBuffer("Dragon","Wind","Dot","Character","Bamboo"))
                                .map(x => fullSuit(x))).flatten

  /* Private function used to initialize the deck.
     Returns 4 of each individual tile in a given
     suit unshuffled */
  private[this] def fullSuit(suit: String): ListBuffer[Tile] = suit match {
      case "Dragon" => val ooe =
                          (ListBuffer(1,2,3)).map(x => new Tile("Dragon", x))
                       (ListBuffer.fill(4)(ooe)).flatten
      case "Wind"   => val ooe =
                          (ListBuffer(1,2,3,4)).map(x => new Tile("Wind", x))
                       (ListBuffer.fill(4)(ooe)).flatten
      case _        => val nmbrs = 1 to 9 toList
                       val curr  = nmbrs.map(x => new Tile(suit,x))
                       (ListBuffer.fill(4)(curr)).flatten
  }

  // !! Should throw an exception when the deck is empty !!
  def draw: Tile = {
    if (deck.length != 0) {
      val top = deck.head
      deck = deck.tail
      return top
    } else {
      throw new DeckEmptyException
    }
  }

  def size = deck.length
  def getDeck = deck
  // Shuffles the current deck.
  def shuffle = { deck = scala.util.Random.shuffle(deck) }

  override def toString: String = deck.toString
}

case class DeckEmptyException(message: String = "Deck is empty",
                cause: Throwable = null)
              extends Exception(message, cause)
