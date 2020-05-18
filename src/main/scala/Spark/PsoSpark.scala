package Spark

import java.io.{BufferedWriter, File, FileWriter}

import Common.BBOFunction
import Entities.{TipoOptimizacion, data}
import ParticleSwarnOptimization.Enjambre

object PsoSpark {
  def crearEnjambre(n_particulas: Int, n_variables: Int, limites_inf: Vector[Double],
                    limites_sup: Vector[Double]): Enjambre = {
    ParticleSwarnOptimization.crearEnjambre(n_particulas,n_variables,limites_inf,limites_sup)
  }

}

class PsoSpark(iterations: Int, func:BBOFunction, inercia: Double, pesoCognitivo: Int, pesoSocial: Int,
               optimizacion: TipoOptimizacion.Optimizacion, funcName: String)
  extends ((Enjambre,Double) => (Enjambre,Vector[data])) with Serializable {


  def apply(enjambre: Enjambre, timeGlobal: Double): (Enjambre,Vector[data]) = {
    var new_enjambre = enjambre
    var i = 0
    var time=timeGlobal
    var result = Vector.fill[data](iterations)(data(0.0,0.0))
    while (i < iterations){
      val initTime = System.nanoTime
      new_enjambre=ParticleSwarnOptimization.optimizar_enjambre(new_enjambre,iterations,func,optimizacion,inercia_max = 0.9,inercia_min = 0.4,
        pesoCognitivo,pesoSocial,i)
      val mejor_valor = new_enjambre.minBy(p => p.mejorValor).mejorValor
      time+=(System.nanoTime - initTime)/1E6
      result = result.updated(i,data(mejor_valor.get,time))
      i+=1
    }
    (new_enjambre,result)
  }
}
