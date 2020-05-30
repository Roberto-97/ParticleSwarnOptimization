package Common

object BBOFunctions {
  val functions = Vector(Ackley, Quadric, Rosenbrock, Schwefel)
  def apply(i: Int): BBOFunction = functions(i)
}

trait BBOFunction extends (Vector[Double] => Double) with Serializable {
  def domainMin: Double
  def domainMax: Double
  def min: Double
}

object Ackley extends BBOFunction {
  def domainMin = -30
  def domainMax = 30
  def min = 0
  def apply(agent: Vector[Double]): Double = {
    var sumsq = 0.0
    var sumcos = 0.0
    for(i <- agent.indices) {
      sumsq += agent(i) * agent(i)
      sumcos += Math.cos(2 *  Math.PI * agent(i))
    }
    val result = -20 * Math.exp(-0.2 * Math.sqrt(sumsq / agent.size)) - Math.exp(sumcos / agent.size) + 20 + Math.E
    result
  }
}

object Griewank extends BBOFunction {
  def domainMin = -600
  def domainMax = 600
  def min = 0
  def apply(agent: Vector[Double]): Double = {
    var sumsq = 0.0
    var prod = 1.0
    for(i <- agent.indices) {
      sumsq += agent(i) * agent(i)
      prod *= Math.cos(agent(i) / Math.sqrt(i + 1))
    }
    1 + sumsq * (1.0/4000.0) - prod
  }
}

object Quadric extends BBOFunction {
  def domainMin = -100
  def domainMax = 100
  def min = 0
  def apply(agent: Vector[Double]): Double = {
    var sumsq = 0.0
    for(i <- agent.indices) {
      var sum = 0.0
      for(j <- 0 to i) {
        sum += agent(j)
      }
      sumsq += sum * sum
    }
    sumsq
  }
}

object Rastrigin extends BBOFunction {
  def domainMin = -5.12
  def domainMax = 5.12
  def min = 0
  def apply(agent: Vector[Double]): Double = {
    var acc = 0.0
    for(i <- agent.indices) {
      acc += agent(i) * agent(i) - 10 * Math.cos(2 * Math.PI * agent(i)) + 10
    }
    acc
  }
}

object Rosenbrock extends BBOFunction {
  def domainMin = -2.048
  def domainMax = 2.048
  def min = 0
  def apply(agent: Vector[Double]): Double = {
    var acc = 0.0
    for(i <- 1 to (agent.size / 2)) {
      val a = agent((i << 1) - 1)
      val b = agent((i << 1) - 2)
      acc += 100 * (a - b * b) * (a - b * b) + (1 - b) * (1 - b)
    }
    acc
  }
}

object Schwefel extends BBOFunction {
  def domainMin = -500
  def domainMax = 500
  def min = 0
  def apply(agent: Vector[Double]): Double = {
    var sum = 0.0
    var outside = false
    for(i <- agent.indices) {
      if(agent(i) < domainMin || agent(i) > domainMax) {
        outside = true
      }
      sum += agent(i) * Math.sin(Math.sqrt(Math.abs(agent(i))))
    }
    sum = agent.size * 4.18982887272434686131e+02 - sum
    if(outside) {
      sum += 1e50
    }
    sum
  }
}
object Spherical extends BBOFunction {
  def domainMin = -100
  def domainMax = 100
  def min = 0
  def apply(agent: Vector[Double]): Double = {
    var sumsq = 0.0
    for(i <- agent.indices) {
      sumsq += agent(i) * agent(i)
    }
    sumsq
  }
}