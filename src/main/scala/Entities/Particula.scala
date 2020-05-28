package Entities

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
