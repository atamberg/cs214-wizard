package apps.app64
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import upickle.default.*

class CardTest extends AnyFlatSpec with Matchers {
  "scoreAgainst" should "return 0 if the current suit is different from the trump suit" in {
    val card = Card(Suit.Hearts, 10)
    val others = List(Card(Suit.Diamonds, 5), Card(Suit.Clubs, 8), Card(Suit.Spades, 3))
    val result = card.scoreAgainst(others, trump = Suit.Spades, current = Suit.Diamonds)
    result should be(0)
  }

  it should "return the card's value if the current suit matches the trump suit" in {
    val card = Card(Suit.Hearts, 10)
    val others = List(Card(Suit.Hearts, 5), Card(Suit.Clubs, 8), Card(Suit.Spades, 3))
    val result = card.scoreAgainst(others, trump = Suit.Hearts, current = Suit.Hearts)
    result should be(1)
  }

  it should "return 0 if the card's suit is different from the current suit and the trump suit" in {
    val card = Card(Suit.Diamonds, 10)
    val others = List(Card(Suit.Hearts, 5), Card(Suit.Clubs, 8), Card(Suit.Spades, 3))
    val result = card.scoreAgainst(others, trump = Suit.Hearts, current = Suit.Clubs)
    result should be(0)
  }

  it should "return the card's value if the card's suit matches the current suit" in {
    val card = Card(Suit.Clubs, 10)
    val others = List(Card(Suit.Hearts, 5), Card(Suit.Clubs, 8), Card(Suit.Spades, 3))
    val result = card.scoreAgainst(others, trump = Suit.Hearts, current = Suit.Clubs)
    result should be(1)
  }
}
