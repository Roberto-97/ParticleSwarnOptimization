package Entities

class Enjambre(var listaParticulas : Vector[Particula], var mejorParticula : Particula) {

  override def toString: String = { "Lista de particulas  = "+ listaParticulas + " \n"+ "mejorParticula = "+ mejorParticula}
}