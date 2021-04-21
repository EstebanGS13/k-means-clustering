import scala.collection.mutable.ArrayDeque
import scala.collection.mutable.Map
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    // Generate a random k
    // val k = defineK(10)
    val k = 3 // TODO change back to random

    // Store data in an array of arrays
    var data = loadData()
    // print(data(0))
    if (k > data.length) return

    // Create a map of k-ArrayDeques
    var clusters = Map.empty[Int, ArrayDeque[Array[Double]]]
    for (i <- Range(0, k)) clusters += (i -> ArrayDeque.empty[Array[Double]])
    //* HashMap(0 -> ArrayDeque(), 1 -> ArrayDeque(), 2 -> ArrayDeque())

    // Create an array of k-arrays, without specifying their dimension
    var centroids = Array.ofDim[Double](k, 0)
    //* Array(Array(), Array(), Array())

    // Fill the centroids array with the first items from data
    //? Can be changed to random too
    for (i <- Range(0, k)) centroids(i) = data(i)

  }

  def defineK(range: Int): Int = {
    var random = scala.util.Random
    val k = random.nextInt(range)
    k
  }

  def loadData(): Array[Array[Double]] = {
    Source
      .fromFile("src/main/resources/data.csv")
      .getLines()
      .map(_.split(",").map(_.trim.toDouble))
      .toArray
  }

}
