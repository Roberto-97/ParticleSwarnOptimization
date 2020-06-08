package Spark


import Common.BBOFunction
import Entities.{Particula, TipoOptimizacion, data}
import Secuencial.PsoSec
import Secuencial.PsoSec.Enjambre

object PsoSpark {
  def crearEnjambre(n_particulas: Int, n_variables: Int, limites_inf: Vector[Double],
                    limites_sup: Vector[Double]): Enjambre = {
    PsoSec.crearEnjambre(n_particulas,n_variables,limites_inf,limites_sup)
  }

}

class PsoSpark(iterations: Int, func:BBOFunction, inercia: Double, pesoCognitivo: Int, pesoSocial: Int,
               optimizacion: TipoOptimizacion.Optimizacion, inercia_max: Double, inercia_min: Double, parada: Double,
               criterio: String)
  extends ((Enjambre,Double) => Option[(Vector[(Int,Particula)],Vector[data])]) with Serializable {


  def optimizar_enjambre(enjambre: Enjambre, n_iteraciones: Int, func: BBOFunction,
                         optimizacion: TipoOptimizacion.Optimizacion, inercia_max: Double, inercia_min: Double, peso_cognitivo: Int,
                         peso_social: Int,i: Int): Enjambre = {
    var new_enjambre = enjambre

    new_enjambre = PsoSec.evaluarEnjambre(new_enjambre, func, optimizacion)

    val new_inercia = (inercia_max - inercia_min) * (n_iteraciones - i) / (n_iteraciones + inercia_min)

    new_enjambre = PsoSec.moverEnjambre(new_enjambre, new_inercia, peso_cognitivo, peso_social)

    new_enjambre
  }


  def apply(enjambre: Enjambre, timeGlobal: Double): Option[(Vector[(Int,Particula)],Vector[data])] = {

    var new_enjambre = enjambre
    var i = 0
    var time=timeGlobal
    var result = Vector[data]()
    var termination = false
    while (if (criterio == "esf") i < iterations else if (criterio == "cal") !termination else (i < iterations && !termination)){
      val initTime = System.nanoTime

      new_enjambre = optimizar_enjambre(new_enjambre,iterations,func,optimizacion,inercia_max ,inercia_min ,
        pesoCognitivo,pesoSocial,i)

      val mejor_valor = new_enjambre.minBy(p => p.mejorValor).mejorValor

      time+=(System.nanoTime - initTime)/1E6

      result = result :+ data(mejor_valor.get,time)
      termination = if (parada >= BigDecimal(mejor_valor.get).setScale(120,BigDecimal.RoundingMode.HALF_UP).toDouble) true else false
      i+=1
    }
    Option((new_enjambre.map(p => (p.id,p)),result))
  }




}
