package IUSec


import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import Common.{Ackley, ExecutionParameters, Quadric, Rastrigin, Spherical}
import Entities.{TipoOptimizacion, data}
import Secuencial.PsoSec
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
    var statistics = Vector.fill[data](20)(data(0.0,0.0))
    var convergence = Seq.fill[data](ep.iterations)(data(0.0,0.0))
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
    outputFile.write("exp"+","+"value"+","+"time"+"\n")
    var cont = 1
    (statistics) map { case value => {
      outputFile.write(cont.toString+","+value.value.toString+","+value.time.toString+"\n")
      cont+=1
    }}
    outputFile.close()
    file = new File(getClass.getClassLoader.getResource("sec"+funcName+".csv").getPath)
    outputFile = new BufferedWriter(new FileWriter(file))
    outputFile.write("iter"+","+"value"+","+"time"+"\n")
    cont=1
    (convergence) map { case value => {
      outputFile.write(cont.toString+","+value.value.toString+","+value.time.toString+"\n")
      cont+=1
    }}
    outputFile.close()
    println("\n Archivo " + funcName + " escrito y preparado para visualización!\n")

  }
}
