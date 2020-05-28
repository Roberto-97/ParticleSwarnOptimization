package Main

import Common.Conf
import Secuencial.PsoSecuencial

object MainSeq {
  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    new PsoSecuencial(conf.build).statistics()
  }
}
