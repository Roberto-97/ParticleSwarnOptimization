package PsoSecInterface


import java.io.{BufferedWriter, File, FileWriter}

import Common.{Ackley, ExecutionParameters, Quadric, Rastrigin, Spherical}
import Entities.{TipoOptimizacion, data}
import Logic.PsoSec
import au.com.bytecode.opencsv.CSVWriter

import scala.collection.JavaConversions._
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
    var convergence = Seq.fill[Double](ep.iterations)(0.0)
    println("***Comienzo del algoritmo***")
    for (i <-0 to 19){
      println("Iteracion ",i)
      val result = PsoSec.optimizar_enjambre(ep.func,ep.n_variables,TipoOptimizacion.minimizar,ep.limit_inf,
        ep.limit_sup,ep.n_particulas,ep.iterations,ep.inercia,true,ep.inercia_max,ep.inercia_min,ep.peso_social,
        ep.peso_cognitivo,false,Some(120),Some(0.000001))
      val value = result(ep.iterations-1)
      if (i == 10){
        convergence = result
      }
      statistics = statistics.updated(i,value)
    }
    var file = new File(getClass.getClassLoader.getResource("secStatistics"+funcName+".csv").getPath)
    var outputFile = new BufferedWriter(new FileWriter(file))
    var csvWriter = new CSVWriter(outputFile)
    var csvFields = Array("value")
    var listOfRecords = new ListBuffer[Array[String]]()
    listOfRecords +=csvFields
    (statistics) map { case value => {
      listOfRecords+= Array(value.toString)
    }}
    var aux  : java.util.List[Array[String]] = listOfRecords.toList
    csvWriter.writeAll(aux)
    outputFile.close()
    file = new File(getClass.getClassLoader.getResource("sec"+funcName+".csv").getPath)
    outputFile = new BufferedWriter(new FileWriter(file))
    csvWriter = new CSVWriter(outputFile)
    csvFields = Array("value")
    listOfRecords = new ListBuffer[Array[String]]()
    listOfRecords +=csvFields
    (convergence) map { case value => {
      listOfRecords+= Array(value.toString)
    }}
    aux = listOfRecords.toList
    csvWriter.writeAll(aux)
    outputFile.close()
    println("\n Archivo " + funcName + " escrito y preparado para visualización!\n")

  }
}
