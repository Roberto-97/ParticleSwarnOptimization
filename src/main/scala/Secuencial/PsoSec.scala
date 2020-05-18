package Secuencial

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}


import Common.{Ackley, BBOFunction,Quadric, Rastrigin, Spherical}
import Entities.{Enjambre, JsonSec, Particula, TipoOptimizacion, data}



object PsoSec {

  def randomValue(min: Double, max:Double): Double ={
    val r = new scala.util.Random
    min + (max - min) * r.nextDouble()
  }

  def crearParticula(n_variables: Int, limites_inf: Vector[Double], limites_sup: Vector[Double])
  : Particula = {

    //Comprobaciones
    require(limites_inf.length == n_variables, "limites_inf debe tener un valor por cada variable.")
    require(limites_sup.length == n_variables, "limites_sup debe tener un valor por cada variable.")

    val posicion = (limites_inf zip limites_sup) map {case (min,max) => {randomValue(min,max)}}

    new Particula(posicion, Vector.fill(n_variables)(0.toDouble))
  }


  def evaluarPartícula(particle: Particula, optimizacion: TipoOptimizacion.Optimizacion, func: BBOFunction): Particula = {

    require((optimizacion.toString == "Maximizar" || optimizacion.toString == "Minimizar"), "El argumento de optimizacion debe ser: maximizar o minimizar")

    particle.valor = Option(func(particle.posicion));

    particle.mejorValor match {
      case None => particle.update(particle.valor, particle.posicion)
      case Some(value) => optimizacion.toString match {
        case "Minimizar" => if (particle.valor.get < value) particle.update(particle.valor, particle.posicion)
        case "Maximizar" => if (particle.valor.get > value) particle.update(particle.valor, particle.posicion)
      }
    }
    particle
  }

  def moverParticula(particle: Particula, mejorPosicion: Vector[Double], inercia: Double,
                     pesoCognitivo: Int, pesoSocial: Int): Particula = {

    val random = new scala.util.Random()
    val r1 = Vector.fill[Double](particle.velocidad.length)(random.nextDouble() * pesoCognitivo)
    val r2 = Vector.fill[Double](particle.velocidad.length)(random.nextDouble() * pesoSocial)
    val componenteVelocidad = particle.velocidad.map(v => inercia * v)
    val componenteCognitivo = productVector(r1,substractVector(particle.mejorPosicion,particle.posicion))
    val componenteSocial = productVector(r2,substractVector(mejorPosicion,particle.posicion))
    val nuevaVelocidad = (componenteVelocidad zip componenteCognitivo zip componenteSocial) map {case ((vel,cog),soc) => vel+cog+soc}
    val nuevaPosicion = sumVector(particle.posicion,nuevaVelocidad)
    particle.velocidad = nuevaVelocidad
    particle.posicion = nuevaPosicion
    particle
  }


  def crearEnjambre(n_particulas: Int, n_variables: Int, limites_inf: Vector[Double],
                    limites_sup: Vector[Double]): Enjambre = {

    val aux_enjambre = Vector.fill[Particula](n_particulas)(crearParticula(n_variables, limites_inf, limites_sup))
    new Enjambre(aux_enjambre, aux_enjambre(0))
  }


  def evaluarEnjambre(enjambre: Enjambre, func: BBOFunction, optimizacion: TipoOptimizacion.Optimizacion)
  : Enjambre = {


    enjambre.listaParticulas = enjambre.listaParticulas.map(particula => evaluarPartícula(particula, optimizacion, func))
    enjambre.mejorParticula = enjambre.listaParticulas.minBy(p => p.mejorValor.get)
    enjambre
  }


  def moverEnjambre(enjambre: Enjambre, inercia: Double, pesoCognitivo: Int, pesoSocial: Int,
                    limites_Inf: Vector[Double], limites_Sup: Vector[Double]): Enjambre = {

    var mejor_posicion_enjambre = Vector.fill[Double](enjambre.listaParticulas(0).posicion.length)(0.0)
    if (enjambre.mejorParticula != null)  mejor_posicion_enjambre = enjambre.mejorParticula.posicion

    enjambre.listaParticulas = enjambre.listaParticulas.map(particula => moverParticula(particula, mejor_posicion_enjambre,
      inercia, pesoCognitivo, pesoSocial))
    enjambre
  }


  def optimizar_enjambre(func: BBOFunction, n_variables: Int, optimizacion: TipoOptimizacion.Optimizacion,
                         limites_inf: Vector[Double], limites_sup: Vector[Double],
                         n_particulas: Int, n_iteraciones: Int, inercia: Double, reduc_inercia: Boolean,
                         inercia_max: Double, inercia_min: Double, peso_cognitivo: Int, peso_social: Int,
                         parada_temprana: Boolean, rondas_parada: Option[Int], tolerancia_parada: Option[Double]): Seq[data] = {


    val startTime = System.nanoTime
    print(" *** Comienzo algoritmo ***\n")

    var historico_enjambre = Seq.fill[data](n_iteraciones)(data(0.0,0.0))


    var enjambre = crearEnjambre(n_particulas, n_variables, limites_inf, limites_sup)
    var time = 0.0
    historico_enjambre = (0 until n_iteraciones) map { case i => {

      val initTime = System.nanoTime
      enjambre = evaluarEnjambre(enjambre, func, optimizacion)

      val new_inercia = (inercia_max - inercia_min) * (n_iteraciones - i) / (n_iteraciones + inercia_min)

      enjambre = moverEnjambre(enjambre, new_inercia, peso_cognitivo, peso_social,
        limites_inf, limites_sup)

      time += (System.nanoTime - initTime)/1E6
      data(enjambre.mejorParticula.mejorValor.get,time)
    }
    }
    print("\n Algoritmo finalizado, tiempo transcurrido: %.0f milisegundos".format((System.nanoTime - startTime)/1E6) + "\n")

    historico_enjambre
  }

}

object testRastrigin extends App {
  print("\n"+ PsoSec.optimizar_enjambre(Rastrigin,8,TipoOptimizacion.minimizar,Vector(-100.0,-100.0,-100.0,-100.0,-100.0,-100.0,-100.0,-100.0),Vector(100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0),500,500,0.8,true,0.9,0.4,2,2,false,Some(120),Some(0.000001)))
}

object testSpherical extends App {
  print("\n"+ PsoSec.optimizar_enjambre(Spherical,8,TipoOptimizacion.minimizar,Vector(-100.0,-100.0,-100.0,-100.0,-100.0,-100.0,-100.0,-100.0),Vector(100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0),150,150,0.8,true,0.9,0.4,2,2,false,Some(120),Some(0.000001)))
}

object testQuadric extends App {
  print("\n"+ PsoSec.optimizar_enjambre(Quadric,8,TipoOptimizacion.minimizar,Vector(-100.0,-100.0,-100.0,-100.0,-100.0,-100.0,-100.0,-100.0),Vector(100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0),500,500,0.8,true,0.9,0.4,2,2,false,Some(120),Some(0.000001)))
}