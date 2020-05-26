package Spark

import java.io.{BufferedWriter, File, FileWriter}

import Common.{Ackley, ExecutionParameters, Quadric, Rastrigin, Spherical}
import Entities.{TipoOptimizacion, data}
import org.apache.spark.{Partitioner, SparkContext}
import org.apache.spark.rdd.RDD
import ParticleSwarnOptimization.Enjambre

class PsoIslands(ep: ExecutionParameters) {

  def run(context : SparkContext): Unit = {
    var funcName = ""
    (ep.func) match {
      case Ackley => funcName = "Ackley"
      case Quadric => funcName = "Quadric"
      case Rastrigin => funcName = "Rastrigin"
      case Spherical => funcName = "Spherical"
    }
    print(" *** Comienzo algoritmo ***\n")
    var beanPlot = Vector.fill[(Int,(data,String))](ep.numberExperiments)(0,(data(0.0,0.0),""))
    var convergence = Vector.fill[(Int,Vector[Vector[data]])](ep.numberExperiments)(0, Vector.fill[Vector[data]](ep.islands)(Vector[data]()))
    var finalConvergence = Vector.fill[Vector[data]](ep.islands)(Vector[data]())
    for (k <- 0 to ep.numberExperiments-1) {
      var infoResults = Vector.fill[Vector[data]](ep.islands)(Vector[data]())
      var population = context.parallelize(PsoSpark.crearEnjambre(ep.n_particulas, ep.n_variables, ep.limit_inf,
        ep.limit_sup)).keyBy(_.id)
      val psoFunction = new PsoSpark(ep.iterations, ep.func, ep.inercia, ep.peso_cognitivo, ep.peso_social,
        TipoOptimizacion.minimizar, funcName)
      var globalIterations = ep.globalIterations
      var best=0.0
      var time = 0.0
      var finalTime = 0.0
      val startTime = System.nanoTime
      var termination = false
      var criterio = ""
      while (if (ep.criterio == "esf") globalIterations > 0 else if (ep.criterio == "cal") !termination else (globalIterations > 0 && !termination)) {
        val initTime = System.nanoTime
        print("Global iteration nº ", globalIterations + "\n")
        val tuple = islands(population, new RandomPartitioner(ep.islands), psoFunction, time)
        population = tuple._1
        best = population.reduce((a, b) => if (a._2.mejorValor.get < b._2.mejorValor.get) a else b)._2.mejorValor.get
        val dataVector = tuple._2.collect().toVector
        for (j <-0 to ep.islands-1){
          var i = 0
          while(i < ep.iterations){
            i+=1
            infoResults =infoResults.updated(j,infoResults(j) :+ dataVector(j)(i-1))
          }
        }
        finalTime = ((System.nanoTime - startTime) / 1E6)
        println("Mejor partícula =>" + best + "\n")
        globalIterations -= 1
        termination = if (ep.parada >= BigDecimal(best).setScale(40,BigDecimal.RoundingMode.HALF_UP).toDouble) true else false
        time += (System.nanoTime - initTime) / 1E6
      }
      if (termination == true) criterio="cal" else criterio="esf"
      beanPlot = beanPlot.updated(k,(k,(data(best,finalTime),criterio)))
      convergence = convergence.updated(k,(k,infoResults))
    }
    beanPlot = beanPlot.sortWith(_._2._1.value < _._2._1.value)
    val median = (beanPlot.size / 2);
    finalConvergence = convergence(beanPlot(median)._1)._2
    var file = new File(ep.islands+"-islands-stat-cc-"+funcName+".csv")
    if (ep.cooperation == 0){
      file = new File(ep.islands+"-islands-stat-sc-"+funcName+".csv")
    }
    var outputFile = new BufferedWriter(new FileWriter(file))
    if (ep.criterio == "both") {
      outputFile.write("exp" + "," + "value" + "," + "time" + "," + "stop" + "\n")
    }
    else {
      outputFile.write("exp" + "," + "value" + "," + "time" + "\n")
    }
    var cont = 1
    (beanPlot) map { case e => {
      if (ep.criterio == "both") {
        outputFile.write(cont.toString + "," + e._2._1.value.toString + "," + e._2._1.time.toString + "," + e._2._2 + "\n")
      }
      else {
        outputFile.write(cont.toString + "," + e._2._1.value.toString + "," + e._2._1.time.toString +"\n")
      }
      cont+=1
    }}
    outputFile.close()
    for(i<-0 to ep.islands-1) {
      file = new File(ep.islands.toString +"-islands-convergence-cc-" + funcName + "-"+i+ ".csv")
      if (ep.cooperation == 0){
        file = new File(ep.islands.toString +"-islands-convergence-sc-"+funcName+"-"+i+".csv")
      }
      outputFile = new BufferedWriter(new FileWriter(file))
      outputFile.write("iter" + "," + "value" + "," + "time" + "\n")
      cont = 1
      (finalConvergence(i)) map { case value => {
        outputFile.write(cont.toString + "," + value.value.toString + "," + value.time.toString + "\n")
        cont += 1
      }
      }
      outputFile.close()
    }
    println("\n Archivo " + funcName + " escrito y preparado para visualización!\n")
  }

  def islands(population: RDD[(Int,Particula)], partitioner: Partitioner, psoFunction: ((Enjambre,Double) => Option[(Enjambre,Vector[data])]), time: Double) : (RDD[(Int,Particula)],RDD[Vector[data]]) = {
    var enjambre= population
    var result = enjambre.partitionBy(partitioner)
      .mapPartitions(p => {
        val info = psoFunction.apply(p.map(_._2).toVector,time).toIterator
        info
      }).cache()
    val rdd = result.map(e => e._1).flatMap(l => l).keyBy(_.id)
    val info = result.map(e => e._2)
    (rdd,info)
  }

}