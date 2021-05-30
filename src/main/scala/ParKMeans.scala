package scala

import concurrencyCommon._

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.ArrayBuffer
import scala.math.sqrt
import scala.io.Source
import scala.util.Random

class ParKMeans(val K: Int, val dataset: String, val partition: Int = 4) {
  val data = loadData()
  var clusters: TrieMap[Int, ArrayBuffer[Array[Double]]] = null
  var centroids = initCentroids()
  var error = 0.0
  val Epsilon = 0

  def predict(iterations: Int, progress: Boolean = false): Unit = {
    if (K > data.length) {
      println("K must be less than the size of the dataset")
      return
    }
    val nthIteration = iterations / 10
    var i = 0
    var end = false
    var previous_sse = 0.0
    while (!end) {
      i += 1
      // emptyClusters()

      // 2. & 3. Assign each point to their closest centroid
      clusters = calculateClusters(0, data.length, data.length / partition)

      // 4. Update each cluster's centroid
      updateCentroids()
      // printInfo()

      // Calculate the squared sum of errors
      val sse = calculateSse()
      error = (previous_sse - sse).abs
      if (error <= Epsilon || i >= iterations) end = true

      if (progress && i % nthIteration == 0)
        println(s"iteration: $i, error: $error")

      previous_sse = sse
      // 5. Repeat
    }
    if (progress)
      println(s"Final\niteration: $i, error: $error")
  }

  def loadData(): Array[Array[Double]] = {
    /* Load data into a 2d-array */
    if (dataset.startsWith("iris")) {
      Source
        .fromFile(s"src/main/resources/$dataset.csv")
        .getLines()
        .drop(1)
        .map(_.split(",").map(_.trim.toDouble))
        .toArray
    } else {
      Source
        .fromFile(s"src/main/resources/$dataset.csv")
        .getLines()
        .map(_.split(",").map(_.trim.toDouble))
        .toArray
    }
  }

  def initClusters(): TrieMap[Int, ArrayBuffer[Array[Double]]] = {
    /* Create a map of k-ArrayBuffers */
    var clusters = TrieMap.empty[Int, ArrayBuffer[Array[Double]]]
    for (i <- 0 until K)
      clusters += (i -> ArrayBuffer[Array[Double]]())
    // TrieMap(0 -> ArrayBuffer(), 1 -> ArrayBuffer(), 2 -> ArrayBuffer(), ...)

    clusters
  }

  def initCentroids(): Array[Array[Double]] = {
    /* Create an array of k-arrays, without specifying their dimension */
    var centroids = Array.ofDim[Double](K, 0)
    // Array(Array(), Array(), Array(), ...)

    // Fill the centroids array with random points from data
    for (i <- 0 until K)
      centroids(i) = randomCentroid(centroids)

    centroids
  }

  def randomCentroid(
      centroids: Array[Array[Double]] = this.centroids
  ): Array[Double] = {
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
    for ((k, v) <- clusters) clusters(k) = ArrayBuffer[Array[Double]]()
  }

  def calculateClusters(
      start: Int,
      end: Int,
      limit: Int
  ): TrieMap[Int, ArrayBuffer[Array[Double]]] = {
    val len = end - start
    if (len <= limit)
      mapRange(start, end)
    else {
      val half = start + (end - start) / 2
      val (temp_clusters1, temp_clusters2) = parallel(
        calculateClusters(start, half, limit),
        calculateClusters(half, end, limit)
      )
      for (i <- 0 until K)
        temp_clusters1(i) ++= temp_clusters2(i)

      temp_clusters1
    }
  }

  def mapRange(
      start: Int,
      end: Int
  ): TrieMap[Int, ArrayBuffer[Array[Double]]] = {
    val temp_clusters = initClusters()
    var i = start
    while (i < end) {
      val point = data(i)
      val centroidId = closestCentroid(point)
      temp_clusters(centroidId) += point
      i += 1
    }
    temp_clusters
  }

  def assignPoint(point: Array[Double]): Unit = {
    /* Assign a given point to the cluster of the closest centroid */
    // Get index of the closest centroid
    val centroidId = closestCentroid(point)
    clusters(centroidId).synchronized {
      clusters(centroidId) += point
    }
  }

  def closestCentroid(point: Array[Double]): Int = {
    /* Find the closest centroid to a given point and return its index */
    var distances = new Array[Double](centroids.length)
    // Store the distances into an array for each centroid
    for (i <- 0 until centroids.length)
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
    for (i <- 0 until point.length) {
      sub = point(i) - centroid(i)
      sum += sub * sub
    }
    sum
  }

  def updateCentroids(): Unit = {
    /* Assign each cluster's mean value as their new centroid */
    for ((centroidId, cluster) <- clusters) {
      // If the cluster is empty, pick a random centroid
      val newCentroid =
        if (cluster.length < 2) randomCentroid() else mean(cluster)
      centroids(centroidId) = newCentroid
    }
  }

  def mean(cluster: ArrayBuffer[Array[Double]]): Array[Double] = {
    /* Calculate the mean value of a given cluster of points */
    // Create an array of zeros with the same length as the first point
    var newCentroid = new Array[Double](cluster(0).length)

    // Calculate the element wise sum of all the points
    for (i <- 0 until cluster.length)
      for (j <- 0 until cluster(i).length)
        newCentroid(j) += cluster(i)(j)

    // Divide each element of the new centroid by the number of points
    for (i <- 0 until newCentroid.length)
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

  def printInfo(printClusters: Boolean = false): Unit = {
    for ((k, v) <- clusters) {
      println(
        s"\nCentroid $k [size = ${centroids(k).length}]:   ${centroids(k)
          .mkString("(", ", ", ")")}"
      )
      print(s"Cluster  $k [size = ${v.length}]: { ")
      if (printClusters)
        for (point <- v) print(s"${point.mkString("(", ", ", ")")}, ")
      println("}\n")
    }
  }
}
