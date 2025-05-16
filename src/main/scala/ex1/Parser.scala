package ex1

/** Consider the Parser example shown in previous lesson. Analogously to
  * NonEmpty, create a mixin NotTwoConsecutive, which adds the idea that one
  * cannot parse two consecutive elements which are equal. Use it (as a mixin)
  * to build class NotTwoConsecutiveParser, used in the testing code at the end.
  * Note we also test that the two mixins can work together!!
  */

abstract class Parser[T]:
  def parse(t: T): Boolean // is the token accepted?
  def end: Boolean // is it ok to end here
  def parseAll(seq: Seq[T]): Boolean =
    (seq forall parse) & end //ogni elemento della sequenza viene passato automaticamente a parse

object Parsers:
  //extension method
  extension (s: String)
    def charParser(): Parser[Char] =
      new BasicParser(s.toSet)

class BasicParser(chars: Set[Char]) extends Parser[Char]:
  override def parse(t: Char): Boolean = chars.contains(t)
  override def end: Boolean = true

trait NonEmpty[T] extends Parser[T]:
  private[this] var empty = true //è una variabile utilizzata per escludere il caso in cui venga restituito true anche quando in realtà la lista è vuota. [this] vuol dire che non può essere acceduta da istanze della classe, create nella classe stessa

  abstract override def parse(t: T): Boolean =
    empty = false; //se viene eseguito parse, vuol dire che la lista contiene almeno un elemento, e quindi empty= false (in modo che sia true quando eseguo end)
    super.parse(t) //richiamo l'implementazione di parse in BasicParser, esteso in NonEmptyParser che utilizza NonEmpty

  abstract override def end: Boolean = !empty && super.end

end NonEmpty

class NonEmptyParser(chars: Set[Char]) extends BasicParser(chars) with NonEmpty[Char]

trait NotTwoConsecutive[T] extends Parser[T]:
  private [this] var first = true
  private [this] var consecutive = false
  private [this] var previous: T = _

  abstract override def parse(t: T): Boolean =
    if(previous == t)
      consecutive = true
    else
      previous = t

    super.parse(t)

  abstract override def end: Boolean = !consecutive && super.end

end NotTwoConsecutive

class NotTwoConsecutiveParser(chars: Set[Char]) extends BasicParser(chars) with NotTwoConsecutive[Char]

@main def checkParsers(): Unit =
  def parser = new BasicParser(Set('a', 'b', 'c')) //ogni volta che chiamo questo metodo, restituisco un oggetto di tipo BasicParser, e siccome BasicParser estende Parser[Char], quell'oggetto ha accesso anche a parseAll.
  //richiamando parseAll viene automaticamente richiamato il metodo parseAll contenuto in Parser, ma con l'implementazione della classe a cui l'istanza appartiene
  println(parser.parseAll("aabc".toList)) // true
  println(parser.parseAll("aabcdc".toList)) // false
  println(parser.parseAll("".toList)) // true
  println("")
  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  def parserNE = new NonEmptyParser(Set('0', '1'))
  println(parserNE.parseAll("0101".toList)) // true
  println(parserNE.parseAll("0123".toList)) // false
  println(parserNE.parseAll(List())) // false
  println("")

  // NotTwoConsecutive[Char] -> BasicParser -> Parser[Char]
  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  println(parserNTC.parseAll("XYZ".toList)) // true
  println(parserNTC.parseAll("XYYZ".toList)) // false
  println(parserNTC.parseAll("".toList)) // true
  println("")

  // note we do not need a class name here, we use the structural type
  // NonEmpty[Char] -> NotTwoConsecutive[Char] -> BasicParser -> Parser[Char]
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
  println(parserNTCNE.parseAll("XYZ".toList)) // true
  println(parserNTCNE.parseAll("XYYZ".toList)) // false
  println(parserNTCNE.parseAll("".toList)) // false
  println("")

  import Parsers.*

  def sparser: Parser[Char] = "abc".charParser()
  println(sparser.parseAll("aabc".toList)) // true
  println(sparser.parseAll("aabcdc".toList)) // false
  println(sparser.parseAll("".toList)) // true
