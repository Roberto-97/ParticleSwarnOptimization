package Spark

import Common.{BBOFunction, ExecutionParameters}
import Entities.{Enjambre, TipoOptimizacion}
import org.apache.log4j.Logger
import org.apache.spark.SparkContext
import Logic.PsoSec.{crearEnjambre, evaluarPartícula, moverEnjambre}

class PsoMasterSlave(ep: ExecutionParameters) {

  @transient lazy val log: Logger = Logger.getLogger(getClass.getName)

  def run(sparkContext : SparkContext) = {
    log.info("****** Optimizando enjambre *******")
    println(optimizar_enjambre(ep.func,ep.n_variables,ep.limit_inf,ep.limit_sup,ep.n_particulas,ep.iterations, ep.inercia,
      ep.inercia_max, ep.inercia_min, ep.peso_cognitivo, ep.peso_social, sparkContext))
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