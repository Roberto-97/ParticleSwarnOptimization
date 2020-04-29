package PsoSecInterface

import Common.Conf

object MainSeq {
  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    new JsonWriter(conf.build).convergence()
  }
}
