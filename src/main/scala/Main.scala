
import scala.collection.*
// a is left limit
// b is right limit
// count is number of bins

case class Bin(startAt: Int, endsAt: Int, elementsCount: Int)

def generateBins(startAt: Int, endsAt: Int, binsCount: Int): List[Bin] = {
  val binXSize = (endsAt - startAt) / binsCount
  generateBinsRec(startAt, binXSize, endsAt, List())
}

def generateBinsRec(startPos: Int, binSize: Int, endPos: Int, res: List[Bin]): List[Bin] = {
  if startPos > endPos then {
    return res
  }

  val newBin = Bin(startPos, startPos + binSize, 0)
  val newStartPos = startPos + binSize
  val newRes = res.appended(newBin)
  generateBinsRec(newStartPos, binSize, endPos, newRes)
}

class Hist private (val a: Int, val b: Int, val count: Int, bins: List[Bin]) 
  extends immutable.Iterable[Any]: 
  self =>

  def this(a: Int, b: Int, count: Int) = {
    this(a, b, count, generateBins(a, b, count))
  }
  // val bins = Range.inclusive(0, this.count - 1).map(x => 0).toList
  // val bins = generateBins(a, b, count)
  // val bins = Range.inclusive(0, this.count - 1).map(x => 0).toList
  // val


  // def Hist(a: Int, b: Int, count: Int) /

  def insert(element: Int): Hist = {
    // val bin = bins.find(bin => bin.startAt =< element && bin.endsAt >= element).get
    // val newBin = new Bin(bin.startAt, bin.endsAt, bin.elementsCount + 1)

    val newBins = bins.map(bin => {
      if bin.startAt < element && bin.endsAt > element then {
        new Bin(bin.startAt, bin.endsAt, bin.elementsCount + 1)
      }
      bin
    })
    Hist(this.a, this.b, this.count, newBins)
  }

  def apply(i: Int): Any = bins(i)

  def iterator: Iterator[Any] = new AbstractIterator[Any]:
    private var current = 0
    def hasNext = current < bins.length

    def next(): Any = 
      val elem = self(current)
      current += 1
      elem
  end iterator


@main def hello: Unit = {
  val h1 = new Hist(5, 0, 10)
  println("Hello world!")
}
