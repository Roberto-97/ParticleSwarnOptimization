package Spark

import Common.ExecutionParameters
import Entities.TipoOptimizacion
import org.apache.spark.{Partitioner, SparkContext}
import org.apache.spark.rdd.RDD
import ParticleSwarnOptimization.Enjambre

class PsoIslands(ep: ExecutionParameters) {

  def run(context : SparkContext): Unit = {
    val startTime = System.nanoTime
    print(" *** Comienzo algoritmo ***\n")
    var population = context.parallelize(PsoSpark.crearEnjambre(ep.n_particulas,ep.n_variables,ep.limit_inf,
      ep.limit_sup)).keyBy(_.id)
    val psoFunction = new PsoSpark(ep.iterations, ep.func, ep.inercia, ep.peso_cognitivo, ep.peso_social,
      TipoOptimizacion.minimizar)
    var globalIterations = ep.globalIterations
    var results = Vector.fill[Double](ep.globalIterations)(0.0)
    var cont = 0;
    while (globalIterations > 0) {
      print("Global iteration nº ",globalIterations+"\n")
      population = islands(population,new RandomPartitioner(ep.islands), psoFunction)
      val mejorParticula = population.map(p => (p._2.mejorValor))
        .reduce((a,b) => if (a.get < b.get) a else b)
      results = results.updated(cont,mejorParticula.get)
      cont +=1
      println("Mejor partícula =>"+ mejorParticula + "\n")
      globalIterations-= 1
    }
    println("Resultado ",results)
    print("\n Algoritmo finalizado, tiempo transcurrido: %.0f milisegundos".format((System.nanoTime - startTime)/1E6) + "\n")

  }

  def islands(population: RDD[(Int,Particula)], partitioner: Partitioner, psoFunction: (Enjambre => Enjambre)) : RDD[(Int,Particula)] = {
    var iterations = ep.islandsIterations
    var enjambre= population
    while (iterations > 0) {
      var iterLoc = 0;
      while (iterLoc <= ep.iterations) {
        enjambre = enjambre.partitionBy(partitioner)
          .mapPartitions(p => psoFunction.apply(p.map(_._2).toVector).toIterator)
          .keyBy(_.id)
        iterLoc+=500
      }
      iterations-=1
    }
    enjambre
  }

}