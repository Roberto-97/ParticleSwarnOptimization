package Spark

import Common.Conf
import org.apache.spark.{SparkConf, SparkContext}


object MainMS {
  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    val sparkConf = new SparkConf().setAppName("pso-spark").setMaster("local["+conf.workers.getOrElse(1)+"]")
    val sc = new SparkContext(sparkConf)
    new PsoMasterSlave(conf.build).statistics(sc)
  }

}