package scala

import org.scalameter._

object Main {
  def main(args: Array[String]): Unit = {
    val K = 4
    val dataset = "digits"
    val iterations = 100
    val progress = false
    val runs = 10

    println("Example: iris dataset")
    example()

    println("Measuring time: digits dataset")
    var timeSeq, timePar = 0.0
    for (i <- 0 until runs) {
      val kmeans = new KMeans(K, dataset)
      val kmeansPar = new ParKMeans(K, dataset)
      // Start with the same centroids
      kmeansPar.centroids = kmeans.centroids.map(_.clone)
      timeSeq += measure(kmeans.predict, iterations, progress)
      timePar += measure(kmeansPar.predict, iterations, progress)
    }
    timeSeq /= runs
    timePar /= runs
    val speedUp = timeSeq / timePar
    println(s"Avg. time seq: $timeSeq ms")
    println(s"Avg. time par: $timePar ms")
    println(f"Speedup: $speedUp%1.4f")
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
