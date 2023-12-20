
import scala.collection.*
// a is left limit
// b is right limit
// count is number of bins

case class Bin(startAt: Int, endsAt: Int, val elementsCount: Int)

def generateBins(binsCount: Int, startAt: Int, endsAt: Int): List[Bin] = {
  val binXSize = (endsAt - startAt) / binsCount
  // println(s"binsCount = $binsCount, startAt = $startAt, endsAt = $endsAt")
  generateBinsRec(startAt, binXSize, endsAt, List())
}

def generateBinsRec(startPos: Int, binSize: Int, endPos: Int, res: List[Bin]): List[Bin] = {
  if startPos >= endPos then {
    // println(s"generatedBins = $res")
    return res
  }
  val newBin = Bin(startPos, startPos + binSize, 0)
  val newStartPos = startPos + binSize
  val newRes = res.appended(newBin)
  generateBinsRec(newStartPos, binSize, endPos, newRes)
}

class Hist private (val count: Int, val a: Int, val b: Int, bins: List[Bin]) 
  extends immutable.Iterable[Bin]: 
  self =>

  def this(count: Int, a: Int, b: Int) = {
    this(a, b, count, generateBins(count, a, b))
  }

  def insert(element: Int): Hist = {
    val newBins = bins.map(bin => {
      if bin.startAt < element && bin.endsAt > element then {
        new Bin(bin.startAt, bin.endsAt, bin.elementsCount + 1)
      }
      bin
    })
    println(s"insert bins = ${bins}")
    println(s"insert newBins = $newBins")
    Hist(this.a, this.b, this.count, newBins)
  }


  def apply(i: Int): Bin = bins(i)

  def iterator: Iterator[Bin] = new AbstractIterator[Bin]:
    private var current = 0
    def hasNext = current < bins.length

    def next(): Bin = 
      val elem = self(current)
      current += 1
      elem
  end iterator

  override def className = "Hist"

  def display() = {
    println(this.bins)
    this.bins.foreach(bin => println(bin))
  }

  override def toString: String  = {
    s"bins = $bins"
  }
end Hist

@main def hello: Unit = {
  val h1 = new Hist(5, 0, 10)
  println(h1)
  // val bins = generateBins(0, 10, 5)
  // println(s"bins = $bins")
  // println(h1)
  // h1.display()
  val h2 = h1.insert(3)
  println(h2)
  val sum = h2.take(3).map(_.elementsCount).sum
  println(sum)
  // h2.display()
  println("Hello world!")
}
