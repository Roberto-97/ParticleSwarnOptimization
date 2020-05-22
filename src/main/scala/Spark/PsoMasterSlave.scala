package Spark

import java.io.{BufferedWriter, File, FileWriter}

import Common.{Ackley, BBOFunction, ExecutionParameters, Quadric, Rastrigin, Spherical}
import Entities.{Enjambre, TipoOptimizacion, data}
import org.apache.log4j.Logger
import org.apache.spark.SparkContext
import Secuencial.PsoSec.{crearEnjambre, evaluarPartícula, moverEnjambre}

import scala.util.Random

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
    var statistics = Vector.fill[(Int,data)](ep.numberExperiments)(0,data(0.0,0.0))
    var convergence = Vector.fill[(Int,Seq[data])](ep.numberExperiments)(0,Vector.fill[data](ep.iterations)(data(0.0,0.0)))
    var finalConvergence = Vector.fill[data](ep.iterations)(data(0.0,0.0))
    for (i <- 0 to ep.numberExperiments-1) {
      println("Iteracion ",i)
      val data = optimizar_enjambre(ep.func, ep.n_variables, ep.limit_inf, ep.limit_sup, ep.n_particulas, ep.iterations, ep.inercia,
        ep.inercia_max, ep.inercia_min, ep.peso_cognitivo, ep.peso_social, sparkContext)
      val value = data(ep.iterations-1)
      statistics = statistics.updated(i,(i,value))
      convergence = convergence.updated(i,(i,data))
    }
    statistics = statistics.sortWith(_._2.value < _._2.value)
    val median = (statistics.size / 2);
    finalConvergence = convergence(statistics(median)._1)._2.toVector
    var file = new File("master-slave-stat-"+funcName+"-1"+".csv")
    var outputFile = new BufferedWriter(new FileWriter(file))
    outputFile.write("exp"+","+"value"+","+"time"+"\n")
    var cont = 1
    (statistics) map { case value => {
      outputFile.write(cont.toString+","+value._2.value.toString+","+value._2.time.toString+"\n")
      cont+=1
    }}
    outputFile.close()
    file = new File("master-slave-"+funcName+"-1"+".csv")
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

  def optimizar_enjambre(func: BBOFunction, n_variables: Int, limites_inf: Vector[Double], limites_sup: Vector[Double],
                         n_particulas: Int, n_iteraciones: Int, inercia: Double,inercia_max: Double, inercia_min: Double,
                         peso_cognitivo: Int, peso_social: Int, sc: SparkContext): Seq[data] = {

    log.info(" *** Comienzo algoritmo ***\n")

    var historico_enjambre = Seq.fill[data](n_iteraciones)(data(0.0,0.0))

    var enjambre = crearEnjambre(n_particulas, n_variables, limites_inf, limites_sup)

    var time = 0.0
    historico_enjambre = (0 until n_iteraciones) map { case i => {
      val startTime = System.nanoTime
      enjambre = evaluarEnjambre(enjambre, func, TipoOptimizacion.minimizar, sc)


      val new_inercia = (inercia_max - inercia_min) * (n_iteraciones - i) / (n_iteraciones + inercia_min)

      enjambre = moverEnjambre(enjambre, new_inercia, peso_cognitivo, peso_social,
        limites_inf, limites_sup)

      time += (System.nanoTime - startTime)/1E6

      data(enjambre.mejorParticula.mejorValor.get,time)
    }
    }
    historico_enjambre
  }
}