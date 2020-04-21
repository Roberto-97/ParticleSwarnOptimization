package Common

case class ExecutionParameters(func : BBOFunction = BBOFunctions(0),
                               n_variables : Int = 10,
                               iterations : Int = 100,
                               n_particulas : Int = 30,
                               limit_inf : Vector[Double] = Vector.fill[Double](10)(-100.0),
                               limit_sup : Vector[Double] = Vector.fill[Double](10)(100.0),
                               inercia : Double = 0.8,
                               inercia_max : Double = 0.9,
                               inercia_min : Double = 0.4,
                               peso_cognitivo : Int = 2,
                               peso_social : Int = 2,
                               islands : Int = 4,
                               globalIterations : Int = 5,
                               islandsIterations : Int = 5
                              )
