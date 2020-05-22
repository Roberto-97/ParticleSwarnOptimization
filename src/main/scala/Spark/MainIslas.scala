package Spark

import Common.Conf
import org.apache.spark.{SparkConf, SparkContext}

object MainIslas {
  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    val sparkConf = new SparkConf().setAppName("pso-spark").setMaster("local[8]")
    val sc = new SparkContext(sparkConf)
    new PsoIslands(conf.build).run(sc)
  }
}
