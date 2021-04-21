import scala.collection.mutable.ArrayDeque
import scala.collection.mutable.Map
import scala.math.sqrt
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    // 1. Generate a random k
    // val k = defineK(10)
    val k = 3 // TODO change back to random

    // Store data in an array of arrays
    var data = loadData()
    print(data(0)(0), data(0)(1))
    if (k > data.length) return

    // Create a map of k-ArrayDeques
    var clusters = Map.empty[Int, ArrayDeque[Array[Double]]]

    // Create an array of k-arrays, without specifying their dimension
    var centroids = Array.ofDim[Double](k, 0)
    //* Array(Array(), Array(), Array())

    for (i <- 0 to k - 1) {
      clusters += (i -> ArrayDeque.empty[Array[Double]])
      //* HashMap(0 -> ArrayDeque(), 1 -> ArrayDeque(), 2 -> ArrayDeque())

      // Fill the centroids array with the first items from data
      centroids(i) = data(i) //? Can be changed to random too
    }

    // 2. & 3. Assign each point to their closest centroid
    for (point <- data) assignPoint(point, centroids, clusters)

    // 4. Update each cluster's centroid
    updateCentroids(centroids, clusters)

    // 5. Repeat

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

  def assignPoint(
      point: Array[Double],
      centroids: Array[Array[Double]],
      clusters: Map[Int, ArrayDeque[Array[Double]]]
  ) {
    val centroidId = closestCentroid(point, centroids)
    clusters(centroidId).append(point)
  }

  def closestCentroid(
      point: Array[Double],
      centroids: Array[Array[Double]]
  ): Int = {
    var distances = new Array[Double](centroids.length)
    for (i <- 0 to centroids.length - 1)
      distances(i) = euclideanDistance(point, centroids(i))

    val centroidId = distances.indexOf(distances.min)
    centroidId
  }

  def euclideanDistance(
      point: Array[Double],
      centroid: Array[Double]
  ): Double = {
    var sum = 0.0
    var d = 0.0
    for (i <- 0 to point.length - 1) {
      d = point(i) - centroid(i)
      sum += d * d
    }
    sqrt(sum)
  }

  def updateCentroids(
      centroids: Array[Array[Double]],
      clusters: Map[Int, ArrayDeque[Array[Double]]]
  ) {
    for ((k, v) <- clusters) {
      val newCentroid = mean(v)
      centroids(k) = newCentroid
    }
  }

  def mean(cluster: ArrayDeque[Array[Double]]): Array[Double] = {
    // if (cluster.empty) return //! ret null?
    var newCentroid = new Array[Double](cluster(0).length)
    for (i <- 0 to cluster.length - 1)
      for (j <- 0 to cluster(i).length - 1)
        newCentroid(j) += cluster(i)(j)

    val numOfPoints = cluster.length
    for (i <- 0 to newCentroid.length - 1)
      newCentroid(i) /= numOfPoints

    newCentroid
  }

}
