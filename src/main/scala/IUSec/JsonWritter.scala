package IUSec


import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import Common.{Ackley, ExecutionParameters, Quadric, Rastrigin, Spherical}
import Entities.{TipoOptimizacion, data}
import Secuencial.PsoSec


class JsonWriter(ep: ExecutionParameters){

  def statistics(): Unit = {
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
    var file = new File("secStatistics"+funcName+".csv")
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
    file = new File("sec"+funcName+".csv")
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
