package Secuencial



import java.io.{BufferedWriter, File, FileWriter}

import Common.{Ackley, BBOFunction, ExecutionParameters, Griewank, Quadric, Rastrigin, Rosenbrock, Schwefel}
import Entities.{Particula, TipoOptimizacion, data}

class PsoSecuencial(ep: ExecutionParameters){

  def statistics(): Unit = {
    var funcName = ""
    (ep.func) match {
      case Ackley => funcName = "Ackley"
      case Quadric => funcName = "Quadric"
      case Rosenbrock => funcName = "Rosenbrock"
      case Rastrigin => funcName = "Rastrigin"
    }
    var statistics = Vector.fill[(Int,(data,String))](ep.numberExperiments)(0,(data(0.0,0.0),""))
    var convergence = Vector[(Int,Seq[data])]()
    var finalConvergence = Vector[data]()
    var criterio = ""
    println("***Comienzo del algoritmo***")
    for (i <-0 to ep.numberExperiments-1){
      println("Iteracion ",i)
      val result = PsoSec.optimizar_enjambre(ep.func,ep.n_variables,TipoOptimizacion.minimizar,ep.limit_inf,
        ep.limit_sup,ep.n_particulas,ep.iterations,ep.inercia,true,ep.inercia_max,ep.inercia_min,ep.peso_social,
        ep.peso_cognitivo, ep.parada,ep.criterio)
      val value = result.last
      if (result.size < ep.iterations) criterio="cal" else criterio="esf"
      statistics = statistics.updated(i,(i,(value,criterio)))
      convergence = convergence :+ (i,(result))
    }
    statistics = statistics.sortWith(_._2._1.value < _._2._1.value)
    val median = (statistics.size / 2);
    finalConvergence = convergence(statistics(median)._1)._2.toVector
    var file = new File("secStatistics"+funcName+ep.criterio+".csv")
    var outputFile = new BufferedWriter(new FileWriter(file))
    if (ep.criterio != "esf") {
      outputFile.write("exp" + "," + "value" + "," + "time" + "," + "stop"+","+"iter" + "\n")
    }
    else {
      outputFile.write("exp" + "," + "value" + "," + "time" + "\n")
    }
    var cont = 1
    (statistics) map { case e => {
      if (ep.criterio != "esf") {
        outputFile.write(cont.toString + "," + e._2._1.value.toString + "," + e._2._1.time.toString + "," + e._2._2 + ","+convergence(e._1)._2.size.toString+ "\n")
      }
      else {
        outputFile.write(cont.toString + "," + e._2._1.value.toString + "," + e._2._1.time.toString +"\n")
      }
      cont+=1
    }}
    outputFile.close()
    file = new File("sec"+funcName+ep.criterio+".csv")
    outputFile = new BufferedWriter(new FileWriter(file))
    outputFile.write("iter"+","+"value"+","+"time"+"\n")
    cont=1
    (finalConvergence) map { case value => {
      outputFile.write(cont.toString+","+value.value.toString+","+value.time.toString+"\n")
      cont+=1
    }}
    outputFile.close()
    println("\n Archivo " + funcName + " escrito y preparado para visualización!\n")

  }
}


object PsoSec {

  type Enjambre = Vector[Particula]

  def randomValue(min: Double, max:Double): Double ={
    val r = new scala.util.Random
    min + (max - min) * r.nextDouble()
  }

  def crearParticula(i: Int,n_variables: Int, limites_inf: Vector[Double], limites_sup: Vector[Double])
  : Particula = {

    //Comprobaciones
    require(limites_inf.length == n_variables, "limites_inf debe tener un valor por cada variable.")
    require(limites_sup.length == n_variables, "limites_sup debe tener un valor por cada variable.")

    val posicion = (limites_inf zip limites_sup) map {case (min,max) => {randomValue(min,max)}}

    new Particula(i,posicion, Vector.fill(n_variables)(0.toDouble))
  }


  def evaluarPartícula(particle: Particula, optimizacion: TipoOptimizacion.Optimizacion, func: BBOFunction): Particula = {

    require((optimizacion.toString == "Maximizar" || optimizacion.toString == "Minimizar"), "El argumento de optimizacion debe ser: maximizar o minimizar")

    particle.valor = Option(func(particle.posicion));

    particle.mejorValor match {
      case None => particle.update(particle.valor, particle.posicion)
      case Some(value) => optimizacion.toString match {
        case "Minimizar" => if (particle.valor.get < value) particle.update(particle.valor, particle.posicion)
        case "Maximizar" => if (particle.valor.get > value) particle.update(particle.valor, particle.posicion)
      }
    }
    particle
  }

  def moverParticula(particle: Particula, mejorPosicion: Vector[Double], inercia: Double,
                     pesoCognitivo: Int, pesoSocial: Int): Particula = {

    val random = new scala.util.Random()
    val r1 = Vector.fill[Double](particle.velocidad.length)(random.nextDouble() * pesoCognitivo)
    val r2 = Vector.fill[Double](particle.velocidad.length)(random.nextDouble() * pesoSocial)
    val componenteVelocidad = particle.velocidad.map(v => inercia * v)
    val componenteCognitivo = productVector(r1,substractVector(particle.mejorPosicion,particle.posicion))
    val componenteSocial = productVector(r2,substractVector(mejorPosicion,particle.posicion))
    val nuevaVelocidad = (componenteVelocidad zip componenteCognitivo zip componenteSocial) map {case ((vel,cog),soc) => vel+cog+soc}
    val nuevaPosicion = sumVector(particle.posicion,nuevaVelocidad)
    particle.velocidad = nuevaVelocidad
    particle.posicion = nuevaPosicion
    particle
  }


  def crearEnjambre(n_particulas: Int, n_variables: Int, limites_inf: Vector[Double],
                    limites_sup: Vector[Double]): Enjambre = {

    ( 0 until n_particulas).map(i => crearParticula(i,n_variables, limites_inf, limites_sup)).toVector
  }


  def evaluarEnjambre(enjambre: Enjambre, func: BBOFunction, optimizacion: TipoOptimizacion.Optimizacion)
  : Enjambre = {
    enjambre.map(particula => evaluarPartícula(particula, optimizacion, func))
  }


  def moverEnjambre(enjambre: Enjambre, inercia: Double, pesoCognitivo: Int, pesoSocial: Int): Enjambre = {

    val mejor_particula = enjambre.minBy(p => p.mejorValor.get)
    enjambre.map(particula => moverParticula(particula, mejor_particula.posicion,
      inercia, pesoCognitivo, pesoSocial))
  }


  def optimizar_enjambre(func: BBOFunction, n_variables: Int, optimizacion: TipoOptimizacion.Optimizacion,
                         limites_inf: Vector[Double], limites_sup: Vector[Double],
                         n_particulas: Int, n_iteraciones: Int, inercia: Double, reduc_inercia: Boolean,
                         inercia_max: Double, inercia_min: Double, peso_cognitivo: Int, peso_social: Int,
                         parada : Double, criterio : String): Seq[data] = {


    val startTime = System.nanoTime
    print(" *** Comienzo algoritmo ***\n")
    var enjambre = crearEnjambre(n_particulas, n_variables, limites_inf, limites_sup)
    var time = 0.0
    var i = 0
    var termination = false
    var historico_enjambre = Vector[data]()
    do{
      val initTime = System.nanoTime

      enjambre = evaluarEnjambre(enjambre, func, optimizacion)

      val new_inercia = (inercia_max - inercia_min) * (n_iteraciones - i) / (n_iteraciones + inercia_min)

      enjambre = moverEnjambre(enjambre, new_inercia, peso_cognitivo, peso_social)


      val mejor_valor = enjambre.minBy(p => p.mejorValor.get).mejorValor.get

      historico_enjambre = historico_enjambre :+ data(mejor_valor,time)

      i+=1

      termination = if (parada >= BigDecimal(mejor_valor).setScale(120,BigDecimal.RoundingMode.HALF_UP).toDouble) true else false
    } while (if (criterio == "esf") i < n_iteraciones else if (criterio == "cal") !termination else (i < n_iteraciones && !termination))

    historico_enjambre
  }

}



