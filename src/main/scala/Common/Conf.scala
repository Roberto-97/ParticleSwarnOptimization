package Common

import org.rogach.scallop.ScallopConf

class Conf(args : Seq[String]) extends ScallopConf(args) with Serializable {
  val function = opt[Int](required = true, validate = f => f >= 0 && f <= 4)
  val n_variables = opt[Int](required = true, validate = _ > 0)
  val iterations = opt[Int](required = true, validate = _ > 0)
  val n_particulas = opt[Int](required = true, validate = _ > 0)
  val islands = opt[Int](required = false, validate = _ > 0)
  val global = opt[Int](required = false, validate = _ > 0)
  val cooperation = opt[Int](required = false,validate = f => f >= 0 && f <= 1)
  verify()

  def build: ExecutionParameters = {
    ExecutionParameters(BBOFunctions(function()),n_variables(),iterations(),n_particulas(),limit_inf = Vector.fill[Double](n_variables())(-100.0),
      limit_sup = Vector.fill[Double](n_variables())(100.0),islands = islands(),globalIterations = global(),cooperation = cooperation())
  }
}
