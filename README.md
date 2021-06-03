# _k_-means clustering

A simple implementation of the _k_-means clustering method in Scala, without using any specialized libraries.
The [K-Means Clustering From Scratch in Python [Algorithm Explained]](https://www.askpython.com/python/examples/k-means-clustering-from-scratch) and the [K Means Clustering Without Libraries](https://towardsdatascience.com/k-means-without-libraries-python-feb3572e2eef) posts were used as a guide to create this implementation.

## Steps

Each one of algorithm's steps and the function used to compute it.

1. Randomly pick k data points as the initial centroids: `initClusters()`.

2. Find the Euclidean distance between each data points in the training set with the k centroids: `calculateClusters()`.

3. Assign each data point to the closest centroid according to the distance found: `assignPoint()`.

4. Update centroid location by taking the average of the points in each cluster group: `updateCentroids()`.

5. Repeat the Steps 2 to 4 till our centroids don’t change: `predict()`.

## Installation

1. Install either [Java](https://www.oracle.com/co/java/technologies/javase-downloads.html) 8, Java 11 or [OpenJDK](https://openjdk.java.net/install/index.html).
2. Install [sbt](https://www.scala-sbt.org/download.html).

Check the Scala [website](https://docs.scala-lang.org/getting-started/index.html) for more details.

## Usage

1. `cd` into `k-means-clustering`.
2. Run the command `sbt`. Make sure to have it added to your `PATH`.
3. After it finishes starting the sbt server, type `run` to compile and run the program.
