package Entities

case class Particula(var posicion : Vector[Double], var velocidad : Vector[Double],
                     var valor : Option[Double], var mejorValor : Option[Double], var mejorPosicion : Vector[Double]) {


  def this(posicion : Vector[Double], velocidad : Vector[Double]) {
    this(posicion,velocidad,None,None,Vector[Double]());
  }

  def update(mejorValor: Option[Double], mejorPosicion: Vector[Double]): Unit ={
    this.mejorValor = mejorValor
    this.mejorPosicion = mejorPosicion
  }

  override def toString: String = ("\n ******  \n"+" Particula : "+"\n"+" Posicion = "+ posicion + "\n"+ " Velocidad = "+ velocidad + "\n"+
    " Valor = "+valor+"\n"+" mejorValor = "+mejorValor+ "\n" + " mejorPosicion = "+ mejorPosicion+ "\n *****")


}
