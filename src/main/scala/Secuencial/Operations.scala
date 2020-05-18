package Secuencial

object productVector extends ((Vector[Double],Vector[Double]) => Vector[Double]){
  def apply(v1 : Vector[Double],v2 : Vector[Double]): Vector[Double] = {
    v1 zip v2 map {case  (e1,e2) => {e1 * e2}}
  }
}

object substractVector extends ((Vector[Double],Vector[Double]) => Vector[Double]){
  def apply(v1 : Vector[Double],v2 : Vector[Double]): Vector[Double] = {
    v1 zip v2 map {case  (e1,e2) => {e1 - e2}}
  }
}

object sumVector extends ((Vector[Double],Vector[Double]) => Vector[Double]){
  def apply(v1 : Vector[Double],v2 : Vector[Double]): Vector[Double] = {
    v1 zip v2 map {case  (e1,e2) => {e1 + e2}}
  }
}

object returnResult extends ((Vector[Double],Vector[Double],Vector[Double],Vector[Double]) => Vector[(Double,Double)]){
  def apply(v1: Vector[Double],v2: Vector[Double], v3: Vector[Double], v4: Vector[Double]): Vector[(Double,Double)] = {
    (v1 zip v2 zip v3 zip v4) map {case (((pos, vel), inf), sup) => {
      if (pos < inf) (inf, 0.0) else (if (pos > sup) (sup, 0.0) else (pos, vel))
    }}
  }
}
