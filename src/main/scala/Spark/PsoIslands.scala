package Spark

import java.io.{BufferedWriter, File, FileWriter}

import Common.{Ackley, ExecutionParameters, Quadric, Rosenbrock, Schwefel}
import Entities.{Particula, TipoOptimizacion, data}
import Secuencial.PsoSec.Enjambre
import org.apache.spark.{Partitioner, SparkContext}
import org.apache.spark.rdd.RDD

class PsoIslands(ep: ExecutionParameters) {

  def run(context : SparkContext): Unit = {
    var funcName = ""
    (ep.func) match {
      case Ackley => funcName = "Ackley"
      case Quadric => funcName = "Quadric"
      case Rosenbrock => funcName = "Rosenbrock"
      case Schwefel => funcName = "Schwefel"
    }
    print(" *** Comienzo algoritmo ***\n")
    val numPartitions = context.defaultParallelism
    var beanPlot = Vector.fill[(Int,(data,String))](ep.numberExperiments)(0,(data(0.0,0.0),""))
    var convergence = Vector.fill[(Int,Vector[Vector[data]])](ep.numberExperiments)(0, Vector.fill[Vector[data]](numPartitions)(Vector[data]()))
    var finalConvergence = Vector.fill[Vector[data]](numPartitions)(Vector[data]())
    var numIters = Vector.fill[Int](ep.numberExperiments)(0)
    for (k <- 0 to ep.numberExperiments-1) {
      var infoResults = Vector.fill[Vector[data]](numPartitions)(Vector[data]())
      val sTime = System.nanoTime
      var population = context.parallelize(PsoSpark.crearEnjambre(ep.n_particulas, ep.n_variables, ep.limit_inf,
        ep.limit_sup)).keyBy(_.id).cache()
      val psoFunction = new PsoSpark(ep.iterations, ep.func, ep.inercia, ep.peso_cognitivo, ep.peso_social,
        TipoOptimizacion.minimizar, ep.inercia_max, ep.inercia_min)
      var globalIterations = ep.globalIterations
      var best=0.0
      var time = 0.0
      val startTime = System.nanoTime
      var finalTime = 0.0
      var termination = false
      var criterio = ""
      var contGlobal = 0
      while (if (ep.criterio == "esf") globalIterations > 0 else if (ep.criterio == "cal") !termination else (globalIterations > 0 && !termination)) {
        val initTime = System.nanoTime
        print("Global iteration nº ", globalIterations + "\n")
        val tuple = islands(population, new RandomPartitioner(numPartitions), psoFunction, time)
        population = tuple._1
        best = population.reduce((a, b) => if (a._2.mejorValor.get < b._2.mejorValor.get) a else b)._2.mejorValor.get
        val dataVector = tuple._2.collect().toVector
        for (j <-0 to numPartitions-1){
          var i = 0
          while(i < ep.iterations){
            i+=1
            infoResults =infoResults.updated(j,infoResults(j) :+ dataVector(j)(i-1))
          }
        }
        finalTime = ((System.nanoTime - startTime) / 1E6)
        println("Mejor partícula =>" + best + "\n")
        globalIterations -= 1
        contGlobal+=1
        termination = if (ep.parada >= BigDecimal(best).setScale(120,BigDecimal.RoundingMode.HALF_UP).toDouble) true else false
        time += (System.nanoTime - initTime) / 1E6
      }
      if (termination == true) criterio="cal" else criterio="esf"
      printf("Final Time -> "+finalTime+"\n")
      beanPlot = beanPlot.updated(k,(k,(data(best,finalTime),criterio)))
      convergence = convergence.updated(k,(k,infoResults))
      numIters = numIters.updated(k,contGlobal)
    }
    beanPlot = beanPlot.sortWith(_._2._1.value < _._2._1.value)
    val median = (beanPlot.size / 2);
    finalConvergence = convergence(beanPlot(median)._1)._2
    var file = new File(numPartitions+"-islands-stat-cc-"+funcName+ep.criterio+".csv")
    if (ep.cooperation == 0){
      file = new File(numPartitions+"-islands-stat-sc-"+funcName+ep.criterio+".csv")
    }
    var outputFile = new BufferedWriter(new FileWriter(file))
    if (ep.criterio != "esf") {
      outputFile.write("exp" + "," + "value" + "," + "time" + "," + "stop" +","+"iter"+"\n")
    }
    else {
      outputFile.write("exp" + "," + "value" + "," + "time" +","+"num_colab"+ "\n")
    }
    var cont = 1
    (beanPlot) map { case elem => {
      if (ep.criterio != "esf") {
        outputFile.write(cont.toString + "," + elem._2._1.value.toString + "," + elem._2._1.time.toString + "," + elem._2._2+","+numIters(elem._1)+ "\n")
      }
      else {
        outputFile.write(cont.toString + "," + elem._2._1.value.toString + "," + elem._2._1.time.toString+","+ep.globalIterations +"\n")
      }
      cont+=1
    }}
    outputFile.close()
    for(i<-0 to numPartitions-1) {
      file = new File(numPartitions.toString +"-islands-convergence-cc-" + funcName + "-"+i+ep.criterio+ ".csv")
      if (ep.cooperation == 0){
        file = new File(numPartitions.toString +"-islands-convergence-sc-"+funcName+"-"+i+ep.criterio+".csv")
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

  def islands(population: RDD[(Int,Particula)], partitioner: Partitioner, psoFunction: ((Enjambre,Double) => Option[(Vector[(Int,Particula)],Vector[data])]), time: Double) : (RDD[(Int,Particula)],RDD[Vector[data]]) = {
    var enjambre = population

    if (ep.cooperation == 1){
      enjambre = enjambre.partitionBy(partitioner)
    }
    val result = enjambre
      .mapPartitions(p => {
        val info = psoFunction.apply(p.map(_._2).toVector,time).toIterator
        info
      }).cache()
    val rdd = result.flatMap(l => l._1)
    val info = result.map(e => e._2)
    (rdd,info)
  }

}