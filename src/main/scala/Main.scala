package scala

import org.scalameter._
import scala.collection.parallel.CollectionConverters._

object Main {
  def main(args: Array[String]): Unit = {
    val K = 4
    val dataset = "digits"
    val iterations = 100
    val progress = false
    val runs = 1

    // println("Example: iris dataset")
    // example()
    // return

    println("Measuring time: digits dataset")
    var timeSeq, timePar, timeColl = 0.0
    for (i <- 0 until runs) {
      val kmeans = new KMeans(K, dataset)
      val kmeansPar = new ParKMeans(K, dataset)
      val parColl = new ParCollKMeans(K, dataset)

      // Start with the same centroids
      kmeansPar.centroids = kmeans.centroids.map(_.clone)
      parColl.centroids = kmeans.centroids.map(_.clone).map(_.par).par

      timeSeq += measure(kmeans.predict, iterations, progress)
      timePar += measure(kmeansPar.predict, iterations, progress)
      timeColl += measure(parColl.predict, iterations, progress)
    }
    timeSeq /= runs
    timePar /= runs
    timeColl /= runs
    val speedUpPar = timeSeq / timePar
    val speedUpColl = timeSeq / timeColl
    println(s"Avg. time seq: $timeSeq ms")
    println(s"Avg. time par: $timePar ms")
    println(s"Avg. time coll: $timeColl ms")
    println(f"Speedup par.: $speedUpPar%1.4f")
    println(f"Speedup coll.: $speedUpColl%1.4f")
  }

  def measure[A, B](func: (A, B) => Unit, i: A, p: B): Double = {
    val time = config(
      Key.exec.benchRuns -> 20
      // Key.verbose -> true
    ) withWarmer {
      new Warmer.Default
    } withMeasurer {
      new Measurer.IgnoringGC
    } measure {
      func(i, p)
    }
    time.value
  }

  def example(): Unit = {
    val kmeans = new KMeans(3, "iris")
    kmeans.predict(100, true)
    kmeans.printInfo(true)
  }
}
