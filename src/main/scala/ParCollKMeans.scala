package scala

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.mutable.ParArray
import scala.math.sqrt
import scala.io.Source
import scala.util.Random

class ParCollKMeans(val K: Int, val dataset: String) {
  val data = loadData()
  var centroids = initCentroids()
  var clusters: ParMap[Int, ParArray[ParArray[Double]]] = null
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

      // 2. & 3. Assign each point to its closest centroid
      calculateClusters()

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

  def loadData(): ParArray[ParArray[Double]] = {
    /* Load data into a 2d-array */
    val data = Source.fromFile(s"src/main/resources/$dataset.csv").getLines()
    if (dataset.startsWith("iris")) data.drop(1)
    data.map(_.split(",").map(_.trim.toDouble)).toArray.map(_.par).par
  }

  def initCentroids(): ParArray[ParArray[Double]] = {
    /* Create an array of k-arrays, without specifying their dimension */
    val centroids = Array.ofDim[Double](K, 0).map(_.par).par
    // Fill the centroids array with random points from data
    centroids.map(i => randomCentroid(centroids))
  }

  def randomCentroid(
      centroids: ParArray[ParArray[Double]] = this.centroids
  ): ParArray[Double] = {
    /* Choose a random point from data, it must not be in centroids already */
    var random: Option[ParArray[Double]] = None
    var unique = false
    while (!unique) {
      random = Some(data(Random.nextInt(data.length)))
      if (!centroids.exists(_.sameElements(random.get))) unique = true
    }
    random.get
  }

  def calculateClusters(): Unit = {
    /* Assign each point to its closest centroid */
    clusters = data.groupBy(closestCentroid)
  }

  def closestCentroid(point: ParArray[Double]): Int = {
    /* Find the closest centroid to a given point and return its index */
    // Store the distances into an array for each centroid
    val distances = centroids.map(i => euclideanDistance(point, i))
    // Get the index of the shortest distance
    distances.indexOf(distances.min)
  }

  def euclideanDistance(
      point: ParArray[Double],
      centroid: ParArray[Double]
  ): Double = {
    /* Calculate the euclidean distance between a point and a centroid */
    sqrt(squaredDistance(point, centroid))
  }

  def squaredDistance(point: ParArray[Double], centroid: ParArray[Double]): Double = {
    (point zip centroid).map { case (p, c) =>
      (p - c) * (p - c)
    }.sum
  }

  def updateCentroids(): Unit = {
    /* Assign each cluster's mean value as their new centroid */
    centroids = (centroids zip clusters).map { case (centroid, (k, v)) =>
      // If the cluster has only one value, pick a random centroid
      if (v.length < 2) randomCentroid() else mean(v)
    }
  }

  def mean(cluster: ParArray[ParArray[Double]]): ParArray[Double] = {
    /* Calculate the mean value of a given cluster of points */
    cluster.transpose.map(_.sum).map(_ / cluster.length)
  }

  def calculateSse(): Double = {
    clusters.map { case (k, v) =>
      v.map(i => squaredDistance(i, centroids(k))).sum
    }.sum
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
