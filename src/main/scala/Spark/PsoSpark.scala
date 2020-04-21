package Spark

import Common.BBOFunction
import Entities.TipoOptimizacion
import ParticleSwarnOptimization.Enjambre

object PsoSpark {
  def crearEnjambre(n_particulas: Int, n_variables: Int, limites_inf: Vector[Double],
                    limites_sup: Vector[Double]): Enjambre = {
    ParticleSwarnOptimization.crearEnjambre(n_particulas,n_variables,limites_inf,limites_sup)
  }

}

class PsoSpark(iterations: Int, func:BBOFunction, inercia: Double, pesoCognitivo: Int, pesoSocial: Int,
               optimizacion: TipoOptimizacion.Optimizacion)
  extends (Enjambre => Enjambre) with Serializable {


  def apply(enjambre: Enjambre): (Enjambre) = {
    var new_enjambre = enjambre
    var i = 0
    while (i != 500){
      new_enjambre = ParticleSwarnOptimization.optimizar_enjambre(new_enjambre,iterations,func, optimizacion, inercia_max = 0.9, inercia_min = 0.4,
        pesoCognitivo, pesoSocial,i)
      i+=1
    }
    (new_enjambre)
  }
}
