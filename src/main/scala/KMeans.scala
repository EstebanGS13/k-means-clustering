package scala

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.math.sqrt
import scala.io.Source
import scala.util.Random

class KMeans(val K: Int, val filename: String) {
  val data = loadData()
  var clusters = initClusters()
  var centroids = initCentroids()
  var error = 0.0
  val Epsilon = 0

  def predict(iterations: Int, progress: Boolean = false): Unit = {
    if (K > data.length) {
      println("K must be less than the size of the dataset")
      return
    }
    var i = 0
    var end = false
    var previous_sse = 0.0
    while (!end) {
      i += 1
      emptyClusters()

      // 2. & 3. Assign each point to their closest centroid
      for (point <- data) assignPoint(point)

      // 4. Update each cluster's centroid
      updateCentroids()
      // printResults()

      // Calculate the squared sum of errors
      val sse = calculateSse()
      error = (previous_sse - sse).abs
      if (error <= Epsilon || i >= iterations) end = true

      // if (progress)
      println(s"i: $i, e: $error")
      // if (i % 10 == 0) println(s"i: $i, e: $error")

      previous_sse = sse
      // 5. Repeat
    }
  }

  def loadData(): Array[Array[Double]] = {
    /* Load data into a 2d-array */
    Source
      .fromFile(s"src/main/resources/$filename.csv")
      .getLines()
      .map(_.split(",").map(_.trim.toDouble))
      .toArray
  }

  def initClusters(): Map[Int, ArrayBuffer[Array[Double]]] = {
    /* Create a map of k-ArrayBuffers */
    var clusters = Map.empty[Int, ArrayBuffer[Array[Double]]]
    for (i <- 0 to K - 1)
      clusters += (i -> ArrayBuffer.empty[Array[Double]])
    // HashMap(0 -> ArrayBuffer(), 1 -> ArrayBuffer(), 2 -> ArrayBuffer(), ...)

    clusters
  }

  def initCentroids(): Array[Array[Double]] = {
    /* Create an array of k-arrays, without specifying their dimension */
    var centroids = Array.ofDim[Double](K, 0)
    // Array(Array(), Array(), Array(), ...)

    // Fill the centroids array with random points from data
    for (i <- 0 to K - 1)
      centroids(i) = randomCentroid(centroids)

    centroids
  }

  def randomCentroid(centroids: Array[Array[Double]] = this.centroids): Array[Double] = {
    /* Choose a random point from data, it must not be in centroids already */
    var random: Option[Array[Double]] = None
    var unique = false
    while (!unique) {
      random = Some(data(Random.nextInt(data.length)))
      if (!centroids.exists(_.sameElements(random.get))) unique = true
    }
    random.get
  }

  def emptyClusters(): Unit = {
    /* Empty each cluster to re-assign the points */
    for ((k, v) <- clusters) clusters(k) = ArrayBuffer.empty[Array[Double]]
  }

  def assignPoint(point: Array[Double]): Unit = {
    /* Assign a given point to the cluster of the closest centroid */
    // Get index of the closest centroid
    val centroidId = closestCentroid(point)
    clusters(centroidId).append(point)
  }

  def closestCentroid(point: Array[Double]): Int = {
    /* Find the closest centroid to a given point and return its index */
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
    sqrt(squaredDistance(point, centroid))
  }

  def squaredDistance(point: Array[Double], centroid: Array[Double]): Double = {
    var sum = 0.0
    var sub = 0.0
    for (i <- 0 to point.length - 1) {
      sub = point(i) - centroid(i)
      sum += sub * sub
    }
    sum
  }

  def updateCentroids(): Unit = {
    /* Assign each cluster's mean value as their new centroid */
    for ((centroidId, cluster) <- clusters) {
      // If the cluster is empty, pick a random centroid
      val newCentroid = if (cluster.length < 2) randomCentroid() else mean(cluster)
      centroids(centroidId) = newCentroid
    }
  }

  def mean(cluster: ArrayBuffer[Array[Double]]): Array[Double] = {
    /* Calculate the mean value of a given cluster of points */
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

  def calculateSse(): Double = {
    var totalSum = 0.0
    for ((centroidId, cluster) <- clusters) {
      var sum = 0.0
      for (point <- cluster)
        sum += squaredDistance(point, centroids(centroidId))

      totalSum += sum
    }
    totalSum
  }

  def printResults(): Unit = {
    for ((k, v) <- clusters) {
      println(s"\nCentroid $k:   ${centroids(k).mkString("(", ", ", ")")}")
      print(s"Cluster  $k: { ")
      for (point <- v) print(s"${point.mkString("(", ", ", ")")}, ")
      println("}\n")
    }
  }
}
