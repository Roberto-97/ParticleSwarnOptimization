package Spark

import java.io.{BufferedWriter, File, FileWriter}

import Common.{Ackley, ExecutionParameters, Quadric, Rastrigin, Spherical}
import Entities.{TipoOptimizacion, data}
import org.apache.spark.{Partitioner, SparkContext}
import org.apache.spark.rdd.RDD
import ParticleSwarnOptimization.Enjambre

class PsoIslands(ep: ExecutionParameters) {

  def run(context : SparkContext): Unit = {
    val startTime = System.nanoTime
    var funcName = ""
    (ep.func) match {
      case Ackley => funcName = "Ackley"
      case Quadric => funcName = "Quadric"
      case Rastrigin => funcName = "Rastrigin"
      case Spherical => funcName = "Spherical"
    }
    print(" *** Comienzo algoritmo ***\n")
    var population = context.parallelize(PsoSpark.crearEnjambre(ep.n_particulas,ep.n_variables,ep.limit_inf,
      ep.limit_sup)).keyBy(_.id)
    val psoFunction = new PsoSpark(ep.iterations, ep.func, ep.inercia, ep.peso_cognitivo, ep.peso_social,
      TipoOptimizacion.minimizar,funcName)
    var globalIterations = ep.globalIterations
    var results = Vector.fill[Double](ep.globalIterations)(0.0)
    var infoResults = Vector.fill[data](ep.globalIterations * ep.iterations)(data(0.0,0.0))
    var cont = 0
    var time = 0.0
    var iterGlobal = 1
    var j=0
    var contIterations = ep.iterations
    while (globalIterations > 0) {
      var i=0;
      val initTime = System.nanoTime
      print("Global iteration nº ",globalIterations+"\n")
      val result = islands(population,new RandomPartitioner(ep.islands), psoFunction)
      population = context.parallelize(result._1).keyBy(_.id)
      while (j < contIterations){
        infoResults = infoResults.updated(j,result._2(i))
        j+=1
        i+=1
      }
      val mejorParticula = population.map(p => (p._2.mejorValor))
        .reduce((a,b) => if (a.get < b.get) a else b).get
      results = results.updated(cont,mejorParticula)
      cont +=1
      println("Mejor partícula =>"+ mejorParticula + "\n")
      iterGlobal += ep.cooperation
      globalIterations-= 1
      contIterations+=ep.iterations
      time+=(System.nanoTime - initTime)/1E6
    }
    //print("Info results", infoResults)
    println("Resultado ",results)
    print("\n Algoritmo finalizado, tiempo transcurrido: %.0f milisegundos".format((System.nanoTime - startTime)/1E6) + "\n")

  }

  def islands(population: RDD[(Int,Particula)], partitioner: Partitioner, psoFunction: (Enjambre => Vector[(Enjambre,Vector[data])])) : (Enjambre,Vector[data]) = {
    var enjambre= population
    val result = enjambre.partitionBy(partitioner)
        .mapPartitions(p => {
          val result = psoFunction.apply(p.map(_._2).toVector).toIterator
          result
        })
    val par = result.collect()
    (par(0)._1,par(0)._2)
  }

}