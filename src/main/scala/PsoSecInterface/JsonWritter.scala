package PsoSecInterface


import java.io.{BufferedWriter, File, FileWriter}
import Common.{Ackley, ExecutionParameters, Quadric, Rastrigin, Spherical}
import Entities.{ TipoOptimizacion}
import Logic.PsoSec
import au.com.bytecode.opencsv.CSVWriter
import scala.collection.JavaConverters._

import scala.collection.mutable.ListBuffer

class JsonWriter(ep: ExecutionParameters){

  def statistics(): Unit = {
    var funcName = ""
    (ep.func) match {
      case Ackley => funcName = "Ackley"
      case Quadric => funcName = "Quadric"
      case Rastrigin => funcName = "Rastrigin"
      case Spherical => funcName = "Spherical"
    }
    var statistics = Vector.fill[Double](20)(0.0)
    println("***Comienzo del algoritmo***")
    for (i <-0 to 19){
      println("Iteracion ",i)
      val result = PsoSec.optimizar_enjambre(ep.func,ep.n_variables,TipoOptimizacion.minimizar,ep.limit_inf,
        ep.limit_sup,ep.n_particulas,ep.iterations,ep.inercia,true,ep.inercia_max,ep.inercia_min,ep.peso_social,
        ep.peso_cognitivo,false,Some(120),Some(0.000001))
      val value = result(ep.iterations-1).value
      statistics = statistics.updated(i,value)
    }
    val file = new File(getClass.getClassLoader.getResource("secStatistics"+funcName+".csv").getPath)
    val outputFile = new BufferedWriter(new FileWriter(file))
    val csvWriter = new CSVWriter(outputFile)
    val csvFields = Array("value")
    var listOfRecords = new ListBuffer[Array[String]]()
    listOfRecords +=csvFields
    (statistics) map { case value => {
      listOfRecords+= Array(value.toString)
    }}
    val aux  : java.util.List[Array[String]] = listOfRecords.toList.asJava
    csvWriter.writeAll(aux)
    outputFile.close()
    println("\n Archivo " + funcName + " escrito y preparado para visualización!\n")

  }
  def convergence() = {
    var funcName = ""
     (ep.func) match {
       case Ackley => funcName = "Ackley"
       case Quadric => funcName = "Quadric"
       case Rastrigin => funcName = "Rastrigin"
       case Spherical => funcName = "Spherical"
    }
    println("***Comienzo algoritmo***")
    println("***"+funcName+" Benchmark***")
    val data = PsoSec.optimizar_enjambre(ep.func,ep.n_variables,TipoOptimizacion.minimizar,ep.limit_inf,
      ep.limit_sup,ep.n_particulas,ep.iterations,ep.inercia,true,ep.inercia_max,ep.inercia_min,ep.peso_social,
      ep.peso_cognitivo,false,Some(120),Some(0.000001))
    println("***Finalizado***")

    val file = new File(getClass.getClassLoader.getResource("sec"+funcName+".csv").getPath)
    val outputFile = new BufferedWriter(new FileWriter(file))
    val csvWriter = new CSVWriter(outputFile)
    val csvFields = Array("value")
    var listOfRecords = new ListBuffer[Array[String]]()
    listOfRecords +=csvFields
    (data) map { case (result) => {
      listOfRecords+= Array(result.value.toString)
    }}
    val aux  : java.util.List[Array[String]] = listOfRecords.toList.asJava
    csvWriter.writeAll(aux)
    outputFile.close()
    println("\n Archivo " + funcName + " escrito y preparado para visualización!\n")

  }
}
