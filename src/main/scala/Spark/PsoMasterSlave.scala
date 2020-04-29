package Spark

import java.io.{BufferedWriter, File, FileWriter}

import Common.{Ackley, BBOFunction, ExecutionParameters, Quadric, Rastrigin, Spherical}
import Entities.{Enjambre, TipoOptimizacion}
import org.apache.log4j.Logger
import org.apache.spark.SparkContext
import Logic.PsoSec.{crearEnjambre, evaluarPartícula, moverEnjambre}
import au.com.bytecode.opencsv.CSVWriter

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

class PsoMasterSlave(ep: ExecutionParameters) {

  @transient lazy val log: Logger = Logger.getLogger(getClass.getName)

  def run(sparkContext : SparkContext) = {
    log.info("****** Optimizando enjambre *******")
    val data = optimizar_enjambre(ep.func,ep.n_variables,ep.limit_inf,ep.limit_sup,ep.n_particulas,ep.iterations, ep.inercia,
      ep.inercia_max, ep.inercia_min, ep.peso_cognitivo, ep.peso_social, sparkContext)
    var funcName = ""
    (ep.func) match {
      case Ackley => funcName = "Ackley"
      case Quadric => funcName = "Quadric"
      case Rastrigin => funcName = "Rastrigin"
      case Spherical => funcName = "Spherical"
    }
    val file = new File(getClass.getClassLoader.getResource("master-slave-"+funcName+"-4"+".csv").getPath)
    val outputFile = new BufferedWriter(new FileWriter(file))
    val csvWriter = new CSVWriter(outputFile)
    val csvFields = Array("value")
    var listOfRecords = new ListBuffer[Array[String]]()
    listOfRecords +=csvFields
    (data) map { case value => {
      listOfRecords+= Array(value.toString)
    }}
    val aux  : java.util.List[Array[String]] = listOfRecords.toList.asJava
    csvWriter.writeAll(aux)
    outputFile.close()
    println("\n Archivo " + funcName + " escrito y preparado para visualización!\n")
  }

  def evaluarEnjambre(enjambre: Enjambre, func: BBOFunction, optimizacion: TipoOptimizacion.Optimizacion, sc: SparkContext)
  : Enjambre = {

    enjambre.listaParticulas = sc.parallelize(enjambre.listaParticulas,4).map(particula => evaluarPartícula(particula, optimizacion, func))
      .collect()
      .toVector
    enjambre.mejorParticula = enjambre.listaParticulas.minBy(p => p.mejorValor.get)
    enjambre
  }

  def optimizar_enjambre(func: BBOFunction, n_variables: Int, limites_inf: Vector[Double], limites_sup: Vector[Double],
                         n_particulas: Int, n_iteraciones: Int, inercia: Double,inercia_max: Double, inercia_min: Double,
                         peso_cognitivo: Int, peso_social: Int, sc: SparkContext): Seq[Double] = {

    val startTime = System.nanoTime
    log.info(" *** Comienzo algoritmo ***\n")

    var historico_enjambre = Seq.fill[(Double, Vector[Double])](n_iteraciones)((0.0, null))


    var enjambre = crearEnjambre(n_particulas, n_variables, limites_inf, limites_sup)

    var diferencia = 0.0

    var timeParallelize = 0.0
    historico_enjambre = (0 until n_iteraciones) map { case i => {
      val startTime = System.nanoTime
      enjambre = evaluarEnjambre(enjambre, func, TipoOptimizacion.minimizar, sc)
      timeParallelize += (System.nanoTime - startTime)/1E6

      if (i > 1) diferencia = (historico_enjambre(i-1)._1 - historico_enjambre(i)._1).abs

      val new_inercia = (inercia_max - inercia_min) * (n_iteraciones - i) / (n_iteraciones + inercia_min)

      enjambre = moverEnjambre(enjambre, new_inercia, peso_cognitivo, peso_social,
        limites_inf, limites_sup)


      (enjambre.mejorParticula.mejorValor.get, enjambre.mejorParticula.mejorPosicion)
    }
    }
    println("Tiempo parallelize : %.0f milisegundos".format(timeParallelize));
    log.info("\n Algoritmo finalizado, tiempo transcurrido: %.0f milisegundos".format((System.nanoTime - startTime)/1E6) + "\n")
    historico_enjambre.unzip._1
  }
}