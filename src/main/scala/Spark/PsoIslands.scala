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
      val tuple = islands(population,new RandomPartitioner(ep.islands), psoFunction, time)
      population = tuple._1
      val best = population.reduce((a,b) => if (a._2.mejorValor.get < b._2.mejorValor.get) a else b)._2.mejorValor.get
      results = results.updated(cont,best)
      cont +=1
      println("Mejor partícula =>"+ best + "\n")
      iterGlobal += ep.cooperation
      globalIterations-= 1
      contIterations+=ep.iterations
      time+=(System.nanoTime - initTime)/1E6
    }
    //print("Info results", infoResults)
    println("Resultado ",results)
    print("\n Algoritmo finalizado, tiempo transcurrido: %.0f milisegundos".format((System.nanoTime - startTime)/1E6) + "\n")

  }

  def islands(population: RDD[(Int,Particula)], partitioner: Partitioner, psoFunction: ((Enjambre,Double) => Option[(Enjambre,Vector[data])]), time: Double) : (RDD[(Int,Particula)],RDD[Vector[data]]) = {
    var enjambre= population
    var result = enjambre.partitionBy(partitioner)
      .mapPartitions(p => {
        val info = psoFunction.apply(p.map(_._2).toVector,time).toIterator
        info
      })
    val rdd = result.map(e => e._1).flatMap(l => l).keyBy(_.id).cache()
    val info = result.map(element => element._2).cache()
    (rdd,info)
  }

}