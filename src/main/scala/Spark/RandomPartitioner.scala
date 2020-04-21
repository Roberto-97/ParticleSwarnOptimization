package Spark

import org.apache.spark.Partitioner

import scala.util.Random


class RandomPartitioner(val numPartitions: Int) extends Partitioner {
  override def getPartition(key: Any): Int = Random.nextInt(numPartitions)
}
