package PsoSecInterface

import Common.ExecutionParameters
import java.io.{BufferedWriter, File, FileWriter}

import com.lambdaworks.jacks.JacksMapper
import Common.{Ackley, BBOFunction, ExecutionParameters, Quadric, Rastrigin, Spherical}
import Entities.{JsonSec, TipoOptimizacion}
import Logic.PsoSec

class JsonWriter(ep: ExecutionParameters){

  def run() = {
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
    val list = JsonSec(funcName+" Benchmark", data)
    println("***Finalizado***")

    var result = Vector[JsonSec](list)
    val json = JacksMapper.writeValueAsString[Vector[JsonSec]](result)
    val file = new File(getClass.getClassLoader.getResource("sec"+funcName+".json").getPath)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(json)
    bw.close()
    println("\n Archivo escrito y preparado para visualización!\n")

  }
}
