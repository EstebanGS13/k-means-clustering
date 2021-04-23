import scala.collection.mutable.ArrayDeque
import scala.collection.mutable.Map
import scala.math.sqrt
import scala.io.Source
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    // Store data in an array of arrays
    var data = loadData("test")

    // 1. Generate a random k
    // val k = randomNumber(10)
    val k = 3 // TODO change back to random

    if (k > data.length) return

    // Create a map of k-ArrayDeques
    var clusters = Map.empty[Int, ArrayDeque[Array[Double]]]

    // Create an array of k-arrays, without specifying their dimension
    var centroids = Array.ofDim[Double](k, 0)
    // Array(Array(), Array(), Array(), ...)

    for (i <- 0 to k - 1) {
      clusters += (i -> ArrayDeque.empty[Array[Double]])
      // HashMap(0 -> ArrayDeque(), 1 -> ArrayDeque(), 2 -> ArrayDeque(), ...)

      // Fill the centroids array with the first items from data
      centroids(i) = data(i) //? Can be changed to random too
    }

    // 2. & 3. Assign each point to their closest centroid
    for (point <- data) assignPoint(point, centroids, clusters)

    // 4. Update each cluster's centroid
    updateCentroids(centroids, clusters)

    // 5. Repeat

  }

  def randomNumber(range: Int): Int = {
    /* Choose a random number from a given range */
    val number = Random.nextInt(range)
    number
  }

  def loadData(filename: String): Array[Array[Double]] = {
    /* Load data into a 2d-array */
    Source
      .fromFile(s"src/main/resources/$filename.csv")
      .getLines()
      .map(_.split(",").map(_.trim.toDouble))
      .toArray
  }

  def assignPoint(
      point: Array[Double],
      centroids: Array[Array[Double]],
      clusters: Map[Int, ArrayDeque[Array[Double]]]
  ): Unit = {
    /* Assign a given point to the cluster of the closest centroid */
    // Get index of the closest centroid
    val centroidId = closestCentroid(point, centroids)
    clusters(centroidId).append(point)
  }

  def closestCentroid(
      point: Array[Double],
      centroids: Array[Array[Double]]
  ): Int = {
    /* Find the closest centroid to a given point and return its index*/
    var distances = new Array[Double](centroids.length)
    // Store the distances into an array for each centroid
    for (i <- 0 to centroids.length - 1)
      distances(i) = euclideanDistance(point, centroids(i))

    // Get the index of the shortest distance
    val centroidId = distances.indexOf(distances.min)
    centroidId
  }

  def euclideanDistance(
      point: Array[Double],
      centroid: Array[Double]
  ): Double = {
    /* Calculate the euclidean distance between a point and a centroid */
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
  ): Unit = {
    /* Assign each cluster's mean value as their new centroid */
    for ((centroidId, cluster) <- clusters) {
      // if (cluster.isEmpty)
      val newCentroid = mean(cluster)
      centroids(centroidId) = newCentroid
    }
  }

  def mean(cluster: ArrayDeque[Array[Double]]): Array[Double] = {
    /* Calculate the mean value of a given cluster of points */
    // if (cluster.empty) return //! ret null?

    // Create an array of zeros with the same length as the first point
    var newCentroid = new Array[Double](cluster(0).length)

    // Calculate the element wise sum of all the points
    for (i <- 0 to cluster.length - 1)
      for (j <- 0 to cluster(i).length - 1)
        newCentroid(j) += cluster(i)(j)

    // Divide each element of the new centroid by the number of points
    for (i <- 0 to newCentroid.length - 1)
      newCentroid(i) /= cluster.length

    newCentroid
  }

  def printResults(
      centroids: Array[Array[Double]],
      clusters: Map[Int, ArrayDeque[Array[Double]]]
  ): Unit = {
    for ((k, v) <- clusters) {
      println(s"\nCentroid $k: ${centroids(k).mkString("[", ", ", "]")}")
      print(s"Cluster: $k { ")
      for (point <- v) print(s"${point.mkString("[", ", ", "]")}, ")
      println("}\n")
    }
  }
}
