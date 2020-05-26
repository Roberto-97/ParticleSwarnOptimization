package Spark

import java.io.{BufferedWriter, File, FileWriter}

import Common.{Ackley, BBOFunction, ExecutionParameters, Quadric, Rastrigin, Spherical}
import Entities.{Enjambre, TipoOptimizacion, data}
import org.apache.log4j.Logger
import org.apache.spark.SparkContext
import Secuencial.PsoSec.{crearEnjambre, evaluarPartícula, moverEnjambre}


class PsoMasterSlave(ep: ExecutionParameters) {

  @transient lazy val log: Logger = Logger.getLogger(getClass.getName)
  def statistics(sparkContext : SparkContext): Unit = {
    var funcName = ""
    (ep.func) match {
      case Ackley => funcName = "Ackley"
      case Quadric => funcName = "Quadric"
      case Rastrigin => funcName = "Rastrigin"
      case Spherical => funcName = "Spherical"
    }
    var statistics = Vector.fill[(Int,(data,String))](ep.numberExperiments)(0,(data(0.0,0.0),""))
    var convergence = Vector[(Int,Seq[data])]()
    var finalConvergence = Vector[data]()
    var criterio = ""
    for (i <- 0 to ep.numberExperiments-1) {
      println("Iteracion ",i)
      val data = optimizar_enjambre(ep.func, ep.n_variables, TipoOptimizacion.minimizar, ep.limit_inf, ep.limit_sup, ep.n_particulas, ep.iterations, ep.inercia,
        ep.inercia_max, ep.inercia_min, ep.peso_cognitivo, ep.peso_social, sparkContext, ep.parada, ep.criterio)
      val value = data.last
      if (data.size < ep.iterations) criterio="cal" else criterio="esf"
      statistics = statistics.updated(i,(i,(value,criterio)))
      convergence = convergence :+ (i,data)
    }
    statistics = statistics.sortWith(_._2._1.value < _._2._1.value)
    val median = (statistics.size / 2);
    finalConvergence = convergence(statistics(median)._1)._2.toVector
    var file = new File("master-slave-stat-"+funcName+"-"+ep.workers+".csv")
    var outputFile = new BufferedWriter(new FileWriter(file))
    if (ep.criterio == "both") {
      outputFile.write("exp" + "," + "value" + "," + "time" + "," + "stop" + "\n")
    }
    else {
      outputFile.write("exp" + "," + "value" + "," + "time" + "\n")
    }
    var cont = 1
    (statistics) map { case e => {
      if (ep.criterio == "both") {
        outputFile.write(cont.toString + "," + e._2._1.value.toString + "," + e._2._1.time.toString + "," + e._2._2 + "\n")
      }
      else {
        outputFile.write(cont.toString + "," + e._2._1.value.toString + "," + e._2._1.time.toString +"\n")
      }
      cont+=1
    }}
    outputFile.close()
    file = new File("master-slave-"+funcName+"-"+ep.workers+".csv")
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

  def evaluarEnjambre(enjambre: Enjambre, func: BBOFunction, optimizacion: TipoOptimizacion.Optimizacion, sc: SparkContext)
  : Enjambre = {

    enjambre.listaParticulas = sc.parallelize(enjambre.listaParticulas)
      .map(particula => evaluarPartícula(particula, optimizacion, func))
      .collect()
      .toVector
    enjambre.mejorParticula = enjambre.listaParticulas.minBy(p => p.mejorValor.get)
    enjambre
  }

  def optimizar_enjambre(func: BBOFunction, n_variables: Int, optimizacion: TipoOptimizacion.Optimizacion, limites_inf: Vector[Double], limites_sup: Vector[Double],
                         n_particulas: Int, n_iteraciones: Int, inercia: Double,inercia_max: Double, inercia_min: Double,
                         peso_cognitivo: Int, peso_social: Int, sc: SparkContext, parada: Double,
                         criterio : String ): Seq[data] = {

    log.info(" *** Comienzo algoritmo ***\n")
    var enjambre = crearEnjambre(n_particulas, n_variables, limites_inf, limites_sup)
    var time = 0.0
    var i = 0
    var termination = false
    var historico_enjambre = Vector[data]()
    do {
      val startTime = System.nanoTime
      enjambre = evaluarEnjambre(enjambre, func, optimizacion, sc)

      val new_inercia = (inercia_max - inercia_min) * (n_iteraciones - i) / (n_iteraciones + inercia_min)

      enjambre = moverEnjambre(enjambre, new_inercia, peso_cognitivo, peso_social,
        limites_inf, limites_sup)
      historico_enjambre = historico_enjambre :+ data(enjambre.mejorParticula.mejorValor.get,time)
      i+=1
      termination = if (parada >= BigDecimal(enjambre.mejorParticula.mejorValor.get).setScale(40,BigDecimal.RoundingMode.HALF_UP).toDouble) true else false
      time += (System.nanoTime - startTime)/1E6
    } while (if (criterio == "esf") i < n_iteraciones else if (criterio == "cal") !termination else (i < n_iteraciones && !termination))
    historico_enjambre
  }
}