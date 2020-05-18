package Spark

import Common.{Ackley, BBOFunction}
import Entities.TipoOptimizacion
import Secuencial.{productVector, substractVector, sumVector}

class Particula(var id: Int, var posicion: Vector[Double], var velocidad: Vector[Double], var mejorPosicion: Vector[Double],
                var valor: Option[Double], var mejorValor: Option[Double]) extends Serializable{
  def this(id: Int,posicion : Vector[Double], velocidad : Vector[Double]) {
    this(id,posicion,velocidad,Vector[Double](),None,None);
  }

  def update(mejorValor: Option[Double], mejorPosicion: Vector[Double]) : Unit = {
    this.mejorValor = mejorValor
    this.mejorPosicion = mejorPosicion
  }
}


object ParticleSwarnOptimization {
  type Enjambre = Vector[Particula]

  def randomValue(min: Double, max:Double): Double ={
    val r = new scala.util.Random
    min + (max - min) * r.nextDouble()
  }

  def crearParticula(id: Int, n_variables: Int, limites_inf: Vector[Double], limites_sup: Vector[Double])
  : Particula = {

    //Comprobaciones
    require(limites_inf.length == n_variables, "limites_inf debe tener un valor por cada variable.")
    require(limites_sup.length == n_variables, "limites_sup debe tener un valor por cada variable.")

    val posicion = (limites_inf zip limites_sup) map {case (min,max) => {randomValue(min,max)}}

    new Particula(id, posicion, Vector.fill(n_variables)(0.toDouble))
  }


  def evaluarPartícula(particle: Particula, optimizacion: TipoOptimizacion.Optimizacion, func: BBOFunction): Particula = {

    require((optimizacion.toString == "Maximizar" || optimizacion.toString == "Minimizar"), "El argumento de optimizacion debe ser: maximizar o minimizar")

    particle.valor = Option(func(particle.posicion));

    particle.mejorValor match {
      case Some(value) => optimizacion.toString match {
        case "Minimizar" => if (particle.valor.get < value) particle.update(particle.valor, particle.posicion)
        case "Maximizar" => if (particle.valor.get > value) particle.update(particle.valor, particle.posicion)
      }
      case None => particle.update(particle.valor, particle.posicion)
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
    ( 0 until n_particulas).map(i => crearParticula(i,n_variables, limites_inf, limites_sup)).toVector
  }


  def evaluarEnjambre(enjambre: Enjambre, func: BBOFunction, optimizacion: TipoOptimizacion.Optimizacion)
  : Enjambre = {
    enjambre.map(particula => evaluarPartícula(particula, optimizacion, func))
  }

  def moverEnjambre(enjambre: Enjambre, inercia: Double, pesoCognitivo: Int, pesoSocial: Int,optimizacion: TipoOptimizacion.Optimizacion): Enjambre = {

    val mejor_particula = enjambre.minBy(p => p.mejorValor.get)
    enjambre.map(particula => moverParticula(particula, mejor_particula.posicion,
      inercia, pesoCognitivo, pesoSocial))
  }

  def optimizar_enjambre(enjambre: Enjambre, n_iteraciones: Int, func: BBOFunction,
                         optimizacion: TipoOptimizacion.Optimizacion, inercia_max: Double, inercia_min: Double, peso_cognitivo: Int,
                         peso_social: Int,i: Int): Enjambre = {

    var new_enjambre = enjambre

    new_enjambre = evaluarEnjambre(new_enjambre, func, optimizacion)

    val new_inercia = (inercia_max - inercia_min) * (n_iteraciones - i) / (n_iteraciones + inercia_min)

    new_enjambre = moverEnjambre(new_enjambre, new_inercia, peso_cognitivo, peso_social, TipoOptimizacion.minimizar)

    new_enjambre
  }
}

object testAckey extends App{
  var enjambre = ParticleSwarnOptimization.crearEnjambre(1200,8,Vector(-100.0,-100.0,-100.0,-100.0,-100.0,-100.0,-100.0,-100.0),Vector(100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0))
  var i=0;
  while ( i < 5){
    var j=0;
    while (j != 50) {
      enjambre = ParticleSwarnOptimization.optimizar_enjambre(enjambre, 1500, Ackley, TipoOptimizacion.minimizar, 0.9, 0.4, 2, 2, i)
      j+=1
    }
      print(enjambre.minBy(p => p.mejorValor.get).mejorValor+"\n")
    i+=1
  }

}
