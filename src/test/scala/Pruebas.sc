import Matrices._
import Benchmark._

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParVector // Para .par en prodPuntoParD

// ========================================================================
// PARTE 1: PRUEBAS FUNCIONALES BÁSICAS (Verificar que compila y da resultados)
// ========================================================================

println("-------------------------------------------------")
println("--- Pruebas para el objeto Matrices ---")
println("-------------------------------------------------")

// Función auxiliar para imprimir matrices de forma legible
def printMatriz(m: Matriz, nombre: String = ""): Unit = {
  if (nombre.nonEmpty) println(s"$nombre:")
  if (m.isEmpty) println(" Vector()")
  else m.foreach(row => println(s" Vector(${row.mkString(", ")})"))
}

def printMatrizPar(m: MatrizPar, nombre: String = ""): Unit = {
  if (nombre.nonEmpty) println(s"$nombre (Par):")
  val mSecuencial = m.seq.map(_.seq) // Convertir todo a secuencial primero

  if (mSecuencial.isEmpty) {
    println(" Vector() (originalmente ParVector())") // Indica que era paralelo
  } else {
    // Imprime la estructura visual de ParVector, pero usando la data secuencial
    // Esto es un poco un "engaño" visual, pero evita el error de pool.
    // O simplemente imprime la versión secuencial directamente.
    println(s"  (Visualización de la estructura original Par):")
    mSecuencial.foreach(row => println(s"  ParVector(${row.mkString(", ")})")) // Usando mSecuencial para la iteración
    // Y luego, si quieres, imprime la versión puramente secuencial como lo hacías
    // printMatriz(mSecuencial, nombre + " (convertida a Seq para visualización)")
  }
}

// Función auxiliar para comparar MatrizPar con Matriz
def sonIguales(mp: MatrizPar, m: Matriz): Boolean = {
  if (mp.isEmpty && m.isEmpty) true
  else if (mp.length != m.length) false
  else if (mp.nonEmpty && m.nonEmpty && (mp.headOption.map(_.length) != m.headOption.map(_.length))) false
  else mp.seq.map(_.seq) == m
}

// Función para calcular transpuesta esperada (para matrices generadas aleatoriamente)
def calcularTranspuestaEsperada(m: Matriz): Matriz = {
  if (m.isEmpty) Vector.empty
  else {
    val rows = m.length
    val cols = m.head.length
    Vector.tabulate(cols, rows)((j, i) => m(i)(j))
  }
}

// Función para calcular producto punto esperado
def calcularProdPuntoEsperado(v1: Vector[Int], v2: Vector[Int]): Int = {
  (v1 zip v2).map { case (x, y) => x * y }.sum
}

// Función para calcular submatriz esperada
def calcularSubMatrizEsperada(m: Matriz, r: Int, c: Int, l: Int): Matriz = {
  Vector.tabulate(l, l)((i, j) => m(r + i)(c + j))
}

// Función para calcular suma de matrices esperada
def calcularSumaMatrizEsperada(m1: Matriz, m2: Matriz): Matriz = {
  Vector.tabulate(m1.length, m1.head.length)((i, j) => m1(i)(j) + m2(i)(j))
}

// Función para calcular resta de matrices esperada
def calcularRestaMatrizEsperada(m1: Matriz, m2: Matriz): Matriz = {
  Vector.tabulate(m1.length, m1.head.length)((i, j) => m1(i)(j) - m2(i)(j))
}


println("=============================================")
println("=== Pruebas para matrizAlAzar ===")
println("=============================================")

// --- Caso 1: Matriz 0x0 ---
val long1_maa = 0
val vals1_maa = 10
val resultado1_maa = matrizAlAzar(long1_maa, vals1_maa)
val esperado1_maa_desc = "Vector()"
println(s"Test 1: Matriz 0x0")
println(s" Long: $long1_maa, Vals: $vals1_maa")
printMatriz(resultado1_maa, "Resultado")
println(s" Esperado (estructura): $esperado1_maa_desc")
println(s" Correcto (estructura): ${resultado1_maa.isEmpty}")
println("-" * 20)

// --- Caso 2: Matriz 1x1 ---
val long2_maa = 1
val vals2_maa = 5
val resultado2_maa = matrizAlAzar(long2_maa, vals2_maa)
println(s"Test 2: Matriz 1x1")
println(s" Long: $long2_maa, Vals: $vals2_maa")
printMatriz(resultado2_maa, "Resultado")
println(s" Esperado (estructura): Matriz 1x1, valores < $vals2_maa")
val correcto2_maa = resultado2_maa.length == 1 && resultado2_maa.head.length == 1 && resultado2_maa.head.head >= 0 && resultado2_maa.head.head < vals2_maa
println(s" Correcto (estructura y rango): $correcto2_maa")
println("-" * 20)

// --- Caso 3: Matriz 3x3 ---
val long3_maa = 3
val vals3_maa = 2
val resultado3_maa = matrizAlAzar(long3_maa, vals3_maa)
println(s"Test 3: Matriz 3x3")
println(s" Long: $long3_maa, Vals: $vals3_maa")
printMatriz(resultado3_maa, "Resultado")
println(s" Esperado (estructura): Matriz 3x3, valores < $vals3_maa")
val correcto3_maa = resultado3_maa.length == 3 && resultado3_maa.forall(_.length == 3) && resultado3_maa.flatten.forall(x => x >= 0 && x < vals3_maa)
println(s" Correcto (estructura y rango): $correcto3_maa")
println("-" * 20)

// --- Caso 4: Matriz con vals = 1 (todos ceros) ---
val long4_maa = 2
val vals4_maa = 1
val resultado4_maa = matrizAlAzar(long4_maa, vals4_maa)
println(s"Test 4: Matriz con vals = 1")
println(s" Long: $long4_maa, Vals: $vals4_maa")
printMatriz(resultado4_maa, "Resultado")
println(s" Esperado (estructura): Matriz 2x2, todos los valores 0")
val correcto4_maa = resultado4_maa.length == 2 && resultado4_maa.forall(_.length == 2) && resultado4_maa.flatten.forall(_ == 0)
println(s" Correcto (estructura y valores): $correcto4_maa")
println("-" * 20)

// --- Caso 5: Matriz 2x2 con vals grande ---
val long5_maa = 2
val vals5_maa = 1000
val resultado5_maa = matrizAlAzar(long5_maa, vals5_maa)
println(s"Test 5: Matriz 2x2 con vals grande")
println(s" Long: $long5_maa, Vals: $vals5_maa")
printMatriz(resultado5_maa, "Resultado")
println(s" Esperado (estructura): Matriz 2x2, valores < $vals5_maa")
val correcto5_maa = resultado5_maa.length == 2 && resultado5_maa.forall(_.length == 2) && resultado5_maa.flatten.forall(x => x >= 0 && x < vals5_maa)
println(s" Correcto (estructura y rango): $correcto5_maa")
println("-" * 20)


println("=============================================")
println("=== Pruebas para vectorAlAzar ===")
println("=============================================")
// --- Caso 1: Vector de longitud 0 ---
val long1_vaa = 0
val vals1_vaa = 10
val resultado1_vaa = vectorAlAzar(long1_vaa, vals1_vaa)
println(s"Test 1: Vector de longitud 0")
println(s" Long: $long1_vaa, Vals: $vals1_vaa")
println(s" Resultado: Vector(${resultado1_vaa.mkString(", ")})")
println(s" Esperado (estructura): Vector()")
println(s" Correcto (estructura): ${resultado1_vaa.isEmpty}")
println("-" * 20)

// --- Caso 2: Vector de longitud 1 ---
val long2_vaa = 1
val vals2_vaa = 5
val resultado2_vaa = vectorAlAzar(long2_vaa, vals2_vaa)
println(s"Test 2: Vector de longitud 1")
println(s" Long: $long2_vaa, Vals: $vals2_vaa")
println(s" Resultado: Vector(${resultado2_vaa.mkString(", ")})")
println(s" Esperado (estructura): Vector de 1 elemento, valor < $vals2_vaa")
val correcto2_vaa = resultado2_vaa.length == 1 && resultado2_vaa.head >= 0 && resultado2_vaa.head < vals2_vaa
println(s" Correcto (estructura y rango): $correcto2_vaa")
println("-" * 20)

// --- Caso 3: Vector de longitud 5 ---
val long3_vaa = 5
val vals3_vaa = 3
val resultado3_vaa = vectorAlAzar(long3_vaa, vals3_vaa)
println(s"Test 3: Vector de longitud 5")
println(s" Long: $long3_vaa, Vals: $vals3_vaa")
println(s" Resultado: Vector(${resultado3_vaa.mkString(", ")})")
println(s" Esperado (estructura): Vector de 5 elementos, valores < $vals3_vaa")
val correcto3_vaa = resultado3_vaa.length == 5 && resultado3_vaa.forall(x => x >= 0 && x < vals3_vaa)
println(s" Correcto (estructura y rango): $correcto3_vaa")
println("-" * 20)

// --- Caso 4: Vector con vals = 1 (todos ceros) ---
val long4_vaa = 4
val vals4_vaa = 1
val resultado4_vaa = vectorAlAzar(long4_vaa, vals4_vaa)
println(s"Test 4: Vector con vals = 1")
println(s" Long: $long4_vaa, Vals: $vals4_vaa")
println(s" Resultado: Vector(${resultado4_vaa.mkString(", ")})")
println(s" Esperado (estructura): Vector de 4 elementos, todos 0")
val correcto4_vaa = resultado4_vaa.length == 4 && resultado4_vaa.forall(_ == 0)
println(s" Correcto (estructura y valores): $correcto4_vaa")
println("-" * 20)

// --- Caso 5: Vector largo con vals grande ---
val long5_vaa = 10
val vals5_vaa = 500
val resultado5_vaa = vectorAlAzar(long5_vaa, vals5_vaa)
println(s"Test 5: Vector largo con vals grande")
println(s" Long: $long5_vaa, Vals: $vals5_vaa")
println(s" Resultado: Vector(${resultado5_vaa.mkString(", ")})")
println(s" Esperado (estructura): Vector de 10 elementos, valores < $vals5_vaa")
val correcto5_vaa = resultado5_vaa.length == 10 && resultado5_vaa.forall(x => x >= 0 && x < vals5_vaa)
println(s" Correcto (estructura y rango): $correcto5_vaa")
println("-" * 20)


println("=============================================")
println("=== Pruebas para transpuesta (y transposeParD) ===")
println("=============================================")

// --- Caso 1: Matriz vacía ---
val m1_t = Vector[Vector[Int]]()
val esperado1_t = calcularTranspuestaEsperada(m1_t) // Vector.empty
val resultado1_t_seq = transpuesta(m1_t)
val resultado1_t_par = transposeParD(transformToParD(m1_t))
println(s"Test 1: Matriz vacía")
printMatriz(m1_t, " Matriz")
printMatriz(esperado1_t, " Esperado")
printMatriz(resultado1_t_seq, " Resultado Seq")
println(s" Correcto Seq: ${resultado1_t_seq == esperado1_t}")
printMatrizPar(resultado1_t_par, " Resultado Par")
println(s" Correcto Par: ${sonIguales(resultado1_t_par, esperado1_t)}")
println("-" * 20)

// --- Caso 2: Matriz 1x1 aleatoria ---
val m2_t = matrizAlAzar(1, 10)
val esperado2_t = calcularTranspuestaEsperada(m2_t)
val resultado2_t_seq = transpuesta(m2_t)
val resultado2_t_par = transposeParD(transformToParD(m2_t))
println(s"Test 2: Matriz 1x1 aleatoria")
printMatriz(m2_t, " Matriz")
printMatriz(esperado2_t, " Esperado")
printMatriz(resultado2_t_seq, " Resultado Seq")
println(s" Correcto Seq: ${resultado2_t_seq == esperado2_t}")
printMatrizPar(resultado2_t_par, " Resultado Par")
println(s" Correcto Par: ${sonIguales(resultado2_t_par, esperado2_t)}")
println("-" * 20)

// --- Caso 3: Matriz cuadrada 2x2 aleatoria ---
val m3_t = matrizAlAzar(2, 5)
val esperado3_t = calcularTranspuestaEsperada(m3_t)
val resultado3_t_seq = transpuesta(m3_t)
val resultado3_t_par = transposeParD(transformToParD(m3_t))
println(s"Test 3: Matriz cuadrada 2x2 aleatoria")
printMatriz(m3_t, " Matriz")
printMatriz(esperado3_t, " Esperado")
printMatriz(resultado3_t_seq, " Resultado Seq")
println(s" Correcto Seq: ${resultado3_t_seq == esperado3_t}")
printMatrizPar(resultado3_t_par, " Resultado Par")
println(s" Correcto Par: ${sonIguales(resultado3_t_par, esperado3_t)}")
println("-" * 20)

// --- Caso 4: Matriz rectangular 3x2 aleatoria ---
val m4_t = Vector.tabulate(3)(_ => vectorAlAzar(2, 10)) // 3 filas, 2 columnas
val esperado4_t = calcularTranspuestaEsperada(m4_t)
val resultado4_t_seq = transpuesta(m4_t)
val resultado4_t_par = transposeParD(transformToParD(m4_t))
println(s"Test 4: Matriz rectangular 3x2 aleatoria")
printMatriz(m4_t, " Matriz")
printMatriz(esperado4_t, " Esperado")
printMatriz(resultado4_t_seq, " Resultado Seq")
println(s" Correcto Seq: ${resultado4_t_seq == esperado4_t}")
printMatrizPar(resultado4_t_par, " Resultado Par")
println(s" Correcto Par: ${sonIguales(resultado4_t_par, esperado4_t)}")
println("-" * 20)

// --- Caso 5: Matriz rectangular 2x3 aleatoria ---
val m5_t = Vector.tabulate(2)(_ => vectorAlAzar(3, 8)) // 2 filas, 3 columnas
val esperado5_t = calcularTranspuestaEsperada(m5_t)
val resultado5_t_seq = transpuesta(m5_t)
val resultado5_t_par = transposeParD(transformToParD(m5_t))
println(s"Test 5: Matriz rectangular 2x3 aleatoria")
printMatriz(m5_t, " Matriz")
printMatriz(esperado5_t, " Esperado")
printMatriz(resultado5_t_seq, " Resultado Seq")
println(s" Correcto Seq: ${resultado5_t_seq == esperado5_t}")
printMatrizPar(resultado5_t_par, " Resultado Par")
println(s" Correcto Par: ${sonIguales(resultado5_t_par, esperado5_t)}")
println("-" * 20)


println("=============================================")
println("=== Pruebas para prodPunto (y prodPuntoParD) ===")
println("=============================================")

// --- Caso 1: Vectores de un elemento aleatorios ---
val v1_1_pp = vectorAlAzar(1, 20)
val v1_2_pp = vectorAlAzar(1, 20)
val esperado1_pp = calcularProdPuntoEsperado(v1_1_pp, v1_2_pp)
val resultado1_pp_seq = prodPunto(v1_1_pp, v1_2_pp)
val resultado1_pp_par = prodPuntoParD(v1_1_pp.par, v1_2_pp.par)
println(s"Test 1: Vectores de un elemento aleatorios")
println(s" Vector1: $v1_1_pp")
println(s" Vector2: $v1_2_pp")
println(s" Esperado: $esperado1_pp")
println(s" Resultado Seq: $resultado1_pp_seq")
println(s" Correcto Seq: ${resultado1_pp_seq == esperado1_pp}")
println(s" Resultado Par: $resultado1_pp_par")
println(s" Correcto Par: ${resultado1_pp_par == esperado1_pp}")
println("-" * 20)

// --- Caso 2: Vectores de múltiples elementos aleatorios ---
val v2_1_pp = vectorAlAzar(3, 10)
val v2_2_pp = vectorAlAzar(3, 10)
val esperado2_pp = calcularProdPuntoEsperado(v2_1_pp, v2_2_pp)
val resultado2_pp_seq = prodPunto(v2_1_pp, v2_2_pp)
val resultado2_pp_par = prodPuntoParD(v2_1_pp.par, v2_2_pp.par)
println(s"Test 2: Vectores de múltiples elementos aleatorios (long 3)")
println(s" Vector1: $v2_1_pp")
println(s" Vector2: $v2_2_pp")
println(s" Esperado: $esperado2_pp")
println(s" Resultado Seq: $resultado2_pp_seq")
println(s" Correcto Seq: ${resultado2_pp_seq == esperado2_pp}")
println(s" Resultado Par: $resultado2_pp_par")
println(s" Correcto Par: ${resultado2_pp_par == esperado2_pp}")
println("-" * 20)

// --- Caso 3: Vectores con ceros (vals = 1 para asegurar ceros) ---
val v3_1_pp = vectorAlAzar(4, 1) // Genera vector de ceros
val v3_2_pp = vectorAlAzar(4, 5)
val esperado3_pp = calcularProdPuntoEsperado(v3_1_pp, v3_2_pp) // Debería ser 0
val resultado3_pp_seq = prodPunto(v3_1_pp, v3_2_pp)
val resultado3_pp_par = prodPuntoParD(v3_1_pp.par, v3_2_pp.par)
println(s"Test 3: Un vector de ceros")
println(s" Vector1: $v3_1_pp")
println(s" Vector2: $v3_2_pp")
println(s" Esperado: $esperado3_pp")
println(s" Resultado Seq: $resultado3_pp_seq")
println(s" Correcto Seq: ${resultado3_pp_seq == esperado3_pp}")
println(s" Resultado Par: $resultado3_pp_par")
println(s" Correcto Par: ${resultado3_pp_par == esperado3_pp}")
println("-" * 20)

// --- Caso 4: Vectores con números potencialmente negativos (no soportado por vectorAlAzar) ---
// Mantendremos este caso estático para probar negativos, ya que vectorAlAzar produce >= 0
val v4_1_pp_static = Vector(-1, 2, -3)
val v4_2_pp_static = Vector(1, -2, 3)
val esperado4_pp = calcularProdPuntoEsperado(v4_1_pp_static, v4_2_pp_static) // -1 - 4 - 9 = -14
val resultado4_pp_seq = prodPunto(v4_1_pp_static, v4_2_pp_static)
val resultado4_pp_par = prodPuntoParD(v4_1_pp_static.par, v4_2_pp_static.par)
println(s"Test 4: Vectores con números negativos (estático)")
println(s" Vector1: $v4_1_pp_static")
println(s" Vector2: $v4_2_pp_static")
println(s" Esperado: $esperado4_pp")
println(s" Resultado Seq: $resultado4_pp_seq")
println(s" Correcto Seq: ${resultado4_pp_seq == esperado4_pp}")
println(s" Resultado Par: $resultado4_pp_par")
println(s" Correcto Par: ${resultado4_pp_par == esperado4_pp}")
println("-" * 20)

// --- Caso 5: Vectores más largos aleatorios ---
val v5_1_pp = vectorAlAzar(10, 5)
val v5_2_pp = vectorAlAzar(10, 5)
val esperado5_pp = calcularProdPuntoEsperado(v5_1_pp, v5_2_pp)
val resultado5_pp_seq = prodPunto(v5_1_pp, v5_2_pp)
val resultado5_pp_par = prodPuntoParD(v5_1_pp.par, v5_2_pp.par)
println(s"Test 5: Vectores más largos aleatorios (long 10)")
println(s" Vector1: $v5_1_pp")
println(s" Vector2: $v5_2_pp")
println(s" Esperado: $esperado5_pp")
println(s" Resultado Seq: $resultado5_pp_seq")
println(s" Correcto Seq: ${resultado5_pp_seq == esperado5_pp}")
println(s" Resultado Par: $resultado5_pp_par")
println(s" Correcto Par: ${resultado5_pp_par == esperado5_pp}")
println("-" * 20)


println("=============================================")
println("=== Pruebas para subMatriz ===")
println("=============================================")
val m_sm_base_dim = 4
val m_sm_base = matrizAlAzar(m_sm_base_dim, 10)
printMatriz(m_sm_base, s"Matriz base ${m_sm_base_dim}x${m_sm_base_dim} aleatoria para subMatriz")

// --- Caso 1: Submatriz 1x1 (esquina superior izquierda) ---
val i1_sm = 0; val j1_sm = 0; val l1_sm = 1
val esperado1_sm = calcularSubMatrizEsperada(m_sm_base, i1_sm, j1_sm, l1_sm)
val resultado1_sm = subMatriz(m_sm_base, i1_sm, j1_sm, l1_sm)
println(s"Test 1: Submatriz 1x1 (0,0)")
println(s" i: $i1_sm, j: $j1_sm, l: $l1_sm")
printMatriz(esperado1_sm, " Esperado")
printMatriz(resultado1_sm, " Resultado")
println(s" Correcto: ${resultado1_sm == esperado1_sm}")
println("-" * 20)

// --- Caso 2: Submatriz 2x2 (esquina superior izquierda) ---
val i2_sm = 0; val j2_sm = 0; val l2_sm = 2
val esperado2_sm = calcularSubMatrizEsperada(m_sm_base, i2_sm, j2_sm, l2_sm)
val resultado2_sm = subMatriz(m_sm_base, i2_sm, j2_sm, l2_sm)
println(s"Test 2: Submatriz 2x2 (0,0)")
println(s" i: $i2_sm, j: $j2_sm, l: $l2_sm")
printMatriz(esperado2_sm, " Esperado")
printMatriz(resultado2_sm, " Resultado")
println(s" Correcto: ${resultado2_sm == esperado2_sm}")
println("-" * 20)

// --- Caso 3: Submatriz 2x2 (centro de 4x4) ---
val i3_sm = 1; val j3_sm = 1; val l3_sm = 2
val esperado3_sm = calcularSubMatrizEsperada(m_sm_base, i3_sm, j3_sm, l3_sm)
val resultado3_sm = subMatriz(m_sm_base, i3_sm, j3_sm, l3_sm)
println(s"Test 3: Submatriz 2x2 (1,1)")
println(s" i: $i3_sm, j: $j3_sm, l: $l3_sm")
printMatriz(esperado3_sm, " Esperado")
printMatriz(resultado3_sm, " Resultado")
println(s" Correcto: ${resultado3_sm == esperado3_sm}")
println("-" * 20)

// --- Caso 4: Submatriz 1x1 (esquina inferior derecha de 4x4) ---
val i4_sm = m_sm_base_dim - 1; val j4_sm = m_sm_base_dim - 1; val l4_sm = 1
val esperado4_sm = calcularSubMatrizEsperada(m_sm_base, i4_sm, j4_sm, l4_sm)
val resultado4_sm = subMatriz(m_sm_base, i4_sm, j4_sm, l4_sm)
println(s"Test 4: Submatriz 1x1 (${i4_sm},${j4_sm})")
println(s" i: $i4_sm, j: $j4_sm, l: $l4_sm")
printMatriz(esperado4_sm, " Esperado")
printMatriz(resultado4_sm, " Resultado")
println(s" Correcto: ${resultado4_sm == esperado4_sm}")
println("-" * 20)

// --- Caso 5: Submatriz igual a la original ---
val i5_sm = 0; val j5_sm = 0; val l5_sm = m_sm_base_dim
val esperado5_sm = calcularSubMatrizEsperada(m_sm_base, i5_sm, j5_sm, l5_sm) // Debería ser m_sm_base
val resultado5_sm = subMatriz(m_sm_base, i5_sm, j5_sm, l5_sm)
println(s"Test 5: Submatriz ${l5_sm}x${l5_sm} (0,0)")
println(s" i: $i5_sm, j: $j5_sm, l: $l5_sm")
printMatriz(esperado5_sm, " Esperado")
printMatriz(resultado5_sm, " Resultado")
println(s" Correcto: ${resultado5_sm == esperado5_sm}")
println("-" * 20)


println("=============================================")
println("=== Pruebas para sumMatriz ===")
println("=============================================")

// --- Caso 1: Matrices 1x1 aleatorias ---
val m1_1_sum = matrizAlAzar(1, 20)
val m1_2_sum = matrizAlAzar(1, 20)
val esperado1_sum = calcularSumaMatrizEsperada(m1_1_sum, m1_2_sum)
val resultado1_sum = sumMatriz(m1_1_sum, m1_2_sum)
println(s"Test 1: Matrices 1x1 aleatorias")
printMatriz(m1_1_sum, " Matriz1")
printMatriz(m1_2_sum, " Matriz2")
printMatriz(esperado1_sum, " Esperado")
printMatriz(resultado1_sum, " Resultado")
println(s" Correcto: ${resultado1_sum == esperado1_sum}")
println("-" * 20)

// --- Caso 2: Matrices 2x2 aleatorias ---
val m2_1_sum = matrizAlAzar(2, 10)
val m2_2_sum = matrizAlAzar(2, 10)
val esperado2_sum = calcularSumaMatrizEsperada(m2_1_sum, m2_2_sum)
val resultado2_sum = sumMatriz(m2_1_sum, m2_2_sum)
println(s"Test 2: Matrices 2x2 aleatorias")
printMatriz(m2_1_sum, " Matriz1")
printMatriz(m2_2_sum, " Matriz2")
printMatriz(esperado2_sum, " Esperado")
printMatriz(resultado2_sum, " Resultado")
println(s" Correcto: ${resultado2_sum == esperado2_sum}")
println("-" * 20)

// --- Caso 3: Suma con matriz de ceros (generada con vals=1) ---
val m3_1_sum = matrizAlAzar(2, 10)
val m3_2_sum = matrizAlAzar(2, 1) // Matriz de ceros
val esperado3_sum = calcularSumaMatrizEsperada(m3_1_sum, m3_2_sum) // Debería ser m3_1_sum
val resultado3_sum = sumMatriz(m3_1_sum, m3_2_sum)
println(s"Test 3: Suma con matriz de ceros (2x2)")
printMatriz(m3_1_sum, " Matriz1")
printMatriz(m3_2_sum, " Matriz2 (ceros)")
printMatriz(esperado3_sum, " Esperado")
printMatriz(resultado3_sum, " Resultado")
println(s" Correcto: ${resultado3_sum == esperado3_sum}")
println("-" * 20)

// --- Caso 4: Suma resultando en algo (negativos no generados por matrizAlAzar) ---
// Para este caso, si queremos resultado específico como ceros con negativos, es mejor estático.
// O, podemos sumar una matriz con su versión "negada" si la tuviéramos.
// Por ahora, solo otra suma aleatoria.
val m4_1_sum = matrizAlAzar(3, 5)
val m4_2_sum = matrizAlAzar(3, 5)
val esperado4_sum = calcularSumaMatrizEsperada(m4_1_sum, m4_2_sum)
val resultado4_sum = sumMatriz(m4_1_sum, m4_2_sum)
println(s"Test 4: Suma de matrices 3x3 aleatorias")
printMatriz(m4_1_sum, " Matriz1")
printMatriz(m4_2_sum, " Matriz2")
printMatriz(esperado4_sum, " Esperado")
printMatriz(resultado4_sum, " Resultado")
println(s" Correcto: ${resultado4_sum == esperado4_sum}")
println("-" * 20)

// --- Caso 5: Matrices rectangulares 2x3 aleatorias ---
val m5_1_sum_filas = 2; val m5_1_sum_cols = 3
val m5_1_sum = Vector.tabulate(m5_1_sum_filas)(_ => vectorAlAzar(m5_1_sum_cols, 10))
val m5_2_sum = Vector.tabulate(m5_1_sum_filas)(_ => vectorAlAzar(m5_1_sum_cols, 10))
val esperado5_sum = calcularSumaMatrizEsperada(m5_1_sum, m5_2_sum)
val resultado5_sum = sumMatriz(m5_1_sum, m5_2_sum)
println(s"Test 5: Matrices 2x3 aleatorias")
printMatriz(m5_1_sum, " Matriz1")
printMatriz(m5_2_sum, " Matriz2")
printMatriz(esperado5_sum, " Esperado")
printMatriz(resultado5_sum, " Resultado")
println(s" Correcto: ${resultado5_sum == esperado5_sum}")
println("-" * 20)


println("=============================================")
println("=== Pruebas para restaMatriz ===")
println("=============================================")

// --- Caso 1: Matrices 1x1 aleatorias ---
val m1_1_res = matrizAlAzar(1, 20)
val m1_2_res = matrizAlAzar(1, 20)
val esperado1_res = calcularRestaMatrizEsperada(m1_1_res, m1_2_res)
val resultado1_res = restaMatriz(m1_1_res, m1_2_res)
println(s"Test 1: Matrices 1x1 aleatorias")
printMatriz(m1_1_res, " Matriz1")
printMatriz(m1_2_res, " Matriz2")
printMatriz(esperado1_res, " Esperado")
printMatriz(resultado1_res, " Resultado")
println(s" Correcto: ${resultado1_res == esperado1_res}")
println("-" * 20)

// --- Caso 2: Matrices 2x2 aleatorias ---
val m2_1_res = matrizAlAzar(2, 10)
val m2_2_res = matrizAlAzar(2, 10)
val esperado2_res = calcularRestaMatrizEsperada(m2_1_res, m2_2_res)
val resultado2_res = restaMatriz(m2_1_res, m2_2_res)
println(s"Test 2: Matrices 2x2 aleatorias")
printMatriz(m2_1_res, " Matriz1")
printMatriz(m2_2_res, " Matriz2")
printMatriz(esperado2_res, " Esperado")
printMatriz(resultado2_res, " Resultado")
println(s" Correcto: ${resultado2_res == esperado2_res}")
println("-" * 20)

// --- Caso 3: Resta de matriz de ceros (generada con vals=1) ---
val m3_1_res = matrizAlAzar(2, 10)
val m3_2_res = matrizAlAzar(2, 1) // Matriz de ceros
val esperado3_res = calcularRestaMatrizEsperada(m3_1_res, m3_2_res) // Debería ser m3_1_res
val resultado3_res = restaMatriz(m3_1_res, m3_2_res)
println(s"Test 3: Resta de matriz de ceros (2x2)")
printMatriz(m3_1_res, " Matriz1")
printMatriz(m3_2_res, " Matriz2 (ceros)")
printMatriz(esperado3_res, " Esperado")
printMatriz(resultado3_res, " Resultado")
println(s" Correcto: ${resultado3_res == esperado3_res}")
println("-" * 20)

// --- Caso 4: Resta de matrices iguales (resultado cero) ---
val m4_1_res = matrizAlAzar(3, 5)
val m4_2_res = m4_1_res // Misma matriz
val esperado4_res = calcularRestaMatrizEsperada(m4_1_res, m4_2_res) // Debería ser matriz de ceros
val resultado4_res = restaMatriz(m4_1_res, m4_2_res)
println(s"Test 4: Resta de matrices iguales (3x3)")
printMatriz(m4_1_res, " Matriz1")
printMatriz(m4_2_res, " Matriz2 (misma que Matriz1)")
printMatriz(esperado4_res, " Esperado (ceros)")
printMatriz(resultado4_res, " Resultado")
println(s" Correcto: ${resultado4_res == esperado4_res}")
println("-" * 20)

// --- Caso 5: Matrices rectangulares 2x3 aleatorias con resta ---
val m5_1_res_filas = 2; val m5_1_res_cols = 3
val m5_1_res = Vector.tabulate(m5_1_res_filas)(_ => vectorAlAzar(m5_1_res_cols, 10))
val m5_2_res = Vector.tabulate(m5_1_res_filas)(_ => vectorAlAzar(m5_1_res_cols, 10))
val esperado5_res = calcularRestaMatrizEsperada(m5_1_res, m5_2_res)
val resultado5_res = restaMatriz(m5_1_res, m5_2_res)
println(s"Test 5: Resta de matrices 2x3 aleatorias")
printMatriz(m5_1_res, " Matriz1")
printMatriz(m5_2_res, " Matriz2")
printMatriz(esperado5_res, " Esperado")
printMatriz(resultado5_res, " Resultado")
println(s" Correcto: ${resultado5_res == esperado5_res}")
println("-" * 20)


println("=============================================")
println("=== Pruebas para transformToParD ===")
println("=============================================")

// --- Caso 1: Matriz vacía ---
val m1_ttpd = Vector[Vector[Int]]()
val resultado1_ttpd = transformToParD(m1_ttpd)
println(s"Test 1: Matriz vacía")
printMatriz(m1_ttpd, "Matriz Original")
printMatrizPar(resultado1_ttpd, "Matriz Transformada")
val esParVector1 = resultado1_ttpd.isInstanceOf[ParVector[ParVector[Int]]]
val contenidoOk1 = resultado1_ttpd.isEmpty
println(s" Es ParVector[ParVector[Int]]: $esParVector1")
println(s" Contenido Correcto (vacío): $contenidoOk1")
println(s" Correcto: ${esParVector1 && contenidoOk1}")
println("-" * 20)

// --- Caso 2: Matriz 1x1 aleatoria ---
val m2_ttpd = matrizAlAzar(1, 10)
val resultado2_ttpd = transformToParD(m2_ttpd)
println(s"Test 2: Matriz 1x1 aleatoria")
printMatriz(m2_ttpd, "Matriz Original")
printMatrizPar(resultado2_ttpd, "Matriz Transformada")
val esParVector2 = resultado2_ttpd.isInstanceOf[ParVector[ParVector[Int]]] && resultado2_ttpd.head.isInstanceOf[ParVector[Int]]
val contenidoOk2 = sonIguales(resultado2_ttpd, m2_ttpd)
println(s" Es ParVector[ParVector[Int]]: $esParVector2")
println(s" Contenido Correcto: $contenidoOk2")
println(s" Correcto: ${esParVector2 && contenidoOk2}")
println("-" * 20)

// --- Caso 3: Matriz 2x2 aleatoria ---
val m3_ttpd = matrizAlAzar(2, 5)
val resultado3_ttpd = transformToParD(m3_ttpd)
println(s"Test 3: Matriz 2x2 aleatoria")
printMatriz(m3_ttpd, "Matriz Original")
printMatrizPar(resultado3_ttpd, "Matriz Transformada")
val esParVector3 = resultado3_ttpd.isInstanceOf[ParVector[ParVector[Int]]] && resultado3_ttpd.forall(_.isInstanceOf[ParVector[Int]])
val contenidoOk3 = sonIguales(resultado3_ttpd, m3_ttpd)
println(s" Es ParVector[ParVector[Int]]: $esParVector3")
println(s" Contenido Correcto (comparando con seq): $contenidoOk3")
println(s" Correcto: ${esParVector3 && contenidoOk3}")
println("-" * 20)

// --- Caso 4: Matriz rectangular 2x1 aleatoria ---
val m4_ttpd = Vector.tabulate(2)(_ => vectorAlAzar(1, 10))
val resultado4_ttpd = transformToParD(m4_ttpd)
println(s"Test 4: Matriz 2x1 aleatoria")
printMatriz(m4_ttpd, "Matriz Original")
printMatrizPar(resultado4_ttpd, "Matriz Transformada")
val esParVector4 = resultado4_ttpd.isInstanceOf[ParVector[ParVector[Int]]] && resultado4_ttpd.forall(_.isInstanceOf[ParVector[Int]])
val contenidoOk4 = sonIguales(resultado4_ttpd, m4_ttpd)
println(s" Es ParVector[ParVector[Int]]: $esParVector4")
println(s" Contenido Correcto (comparando con seq): $contenidoOk4")
println(s" Correcto: ${esParVector4 && contenidoOk4}")
println("-" * 20)

// --- Caso 5: Matriz rectangular 1x2 aleatoria ---
val m5_ttpd = Vector.tabulate(1)(_ => vectorAlAzar(2, 10))
val resultado5_ttpd = transformToParD(m5_ttpd)
println(s"Test 5: Matriz 1x2 aleatoria")
printMatriz(m5_ttpd, "Matriz Original")
printMatrizPar(resultado5_ttpd, "Matriz Transformada")
val esParVector5 = resultado5_ttpd.isInstanceOf[ParVector[ParVector[Int]]] && resultado5_ttpd.forall(_.isInstanceOf[ParVector[Int]])
val contenidoOk5 = sonIguales(resultado5_ttpd, m5_ttpd)
println(s" Es ParVector[ParVector[Int]]: $esParVector5")
println(s" Contenido Correcto (comparando con seq): $contenidoOk5")
println(s" Correcto: ${esParVector5 && contenidoOk5}")
println("-" * 20)


println("=============================================")
println("=== Pruebas para multMatrizParD ===")
println("=============================================")

// --- Caso 1: Multiplicación 1x1 aleatoria ---
val m1_1_mmpd_orig = matrizAlAzar(1, 10)
val m1_2_mmpd_orig = matrizAlAzar(1, 10)
val m1_1_mmpd = transformToParD(m1_1_mmpd_orig)
val m1_2_mmpd = transformToParD(m1_2_mmpd_orig)
val esperado1_mmpd = multMatriz(m1_1_mmpd_orig, m1_2_mmpd_orig)
val resultado1_mmpd = multMatrizParD(m1_1_mmpd, m1_2_mmpd)
println(s"Test 1: Multiplicación 1x1 aleatoria (ParD)")
printMatriz(m1_1_mmpd_orig, " Matriz1 Original")
printMatriz(m1_2_mmpd_orig, " Matriz2 Original")
printMatriz(esperado1_mmpd, " Esperado (de multMatriz)")
printMatrizPar(resultado1_mmpd, " Resultado ParD")
println(s" Correcto: ${sonIguales(resultado1_mmpd, esperado1_mmpd)}")
println("-" * 20)

// --- Caso 2: Multiplicación 2x2 aleatoria ---
val m2_1_mmpd_orig = matrizAlAzar(2, 5)
val m2_2_mmpd_orig = matrizAlAzar(2, 5)
val m2_1_mmpd = transformToParD(m2_1_mmpd_orig)
val m2_2_mmpd = transformToParD(m2_2_mmpd_orig)
val esperado2_mmpd = multMatriz(m2_1_mmpd_orig, m2_2_mmpd_orig)
val resultado2_mmpd = multMatrizParD(m2_1_mmpd, m2_2_mmpd)
println(s"Test 2: Multiplicación 2x2 aleatoria (ParD)")
printMatriz(m2_1_mmpd_orig, " Matriz1 Original")
printMatriz(m2_2_mmpd_orig, " Matriz2 Original")
printMatriz(esperado2_mmpd, " Esperado (de multMatriz)")
printMatrizPar(resultado2_mmpd, " Resultado ParD")
println(s" Correcto: ${sonIguales(resultado2_mmpd, esperado2_mmpd)}")
println("-" * 20)

// --- Caso 3: Multiplicación 2x3 por 3x2 -> 2x2 (aleatoria) ---
val m3_1_mmpd_orig = Vector.tabulate(2)(_ => vectorAlAzar(3, 4)) // 2x3
val m3_2_mmpd_orig = Vector.tabulate(3)(_ => vectorAlAzar(2, 4)) // 3x2
val m3_1_mmpd = transformToParD(m3_1_mmpd_orig)
val m3_2_mmpd = transformToParD(m3_2_mmpd_orig)
val esperado3_mmpd = multMatriz(m3_1_mmpd_orig, m3_2_mmpd_orig)
val resultado3_mmpd = multMatrizParD(m3_1_mmpd, m3_2_mmpd)
println(s"Test 3: Multiplicación 2x3 por 3x2 aleatoria (ParD)")
printMatriz(m3_1_mmpd_orig, " Matriz1 Original")
printMatriz(m3_2_mmpd_orig, " Matriz2 Original")
printMatriz(esperado3_mmpd, " Esperado (de multMatriz)")
printMatrizPar(resultado3_mmpd, " Resultado ParD")
println(s" Correcto: ${sonIguales(resultado3_mmpd, esperado3_mmpd)}")
println("-" * 20)

// --- Caso 4: Multiplicación por identidad 2x2 (aleatoria * identidad) ---
val m4_1_mmpd_orig = matrizAlAzar(2, 7)
val m4_2_mmpd_orig_id = Vector(Vector(1, 0), Vector(0, 1)) // Identidad
val m4_1_mmpd = transformToParD(m4_1_mmpd_orig)
val m4_2_mmpd = transformToParD(m4_2_mmpd_orig_id)
val esperado4_mmpd = multMatriz(m4_1_mmpd_orig, m4_2_mmpd_orig_id) // Debería ser m4_1_mmpd_orig
val resultado4_mmpd = multMatrizParD(m4_1_mmpd, m4_2_mmpd)
println(s"Test 4: Multiplicación por identidad 2x2 (ParD)")
printMatriz(m4_1_mmpd_orig, " Matriz1 Original")
printMatriz(m4_2_mmpd_orig_id, " Matriz2 Original (Identidad)")
printMatriz(esperado4_mmpd, " Esperado (de multMatriz)")
printMatrizPar(resultado4_mmpd, " Resultado ParD")
println(s" Correcto: ${sonIguales(resultado4_mmpd, esperado4_mmpd)}")
println("-" * 20)

// --- Caso 5: Multiplicación con una matriz de ceros 2x2 ---
val m5_1_mmpd_orig = matrizAlAzar(2, 6)
val m5_2_mmpd_orig_zeros = matrizAlAzar(2, 1) // Matriz de ceros
val m5_1_mmpd = transformToParD(m5_1_mmpd_orig)
val m5_2_mmpd = transformToParD(m5_2_mmpd_orig_zeros)
val esperado5_mmpd = multMatriz(m5_1_mmpd_orig, m5_2_mmpd_orig_zeros) // Debería ser matriz de ceros
val resultado5_mmpd = multMatrizParD(m5_1_mmpd, m5_2_mmpd)
println(s"Test 5: Multiplicación con matriz de ceros 2x2 (ParD)")
printMatriz(m5_1_mmpd_orig, " Matriz1 Original")
printMatriz(m5_2_mmpd_orig_zeros, " Matriz2 Original (Ceros)")
printMatriz(esperado5_mmpd, " Esperado (de multMatriz)")
printMatrizPar(resultado5_mmpd, " Resultado ParD")
println(s" Correcto: ${sonIguales(resultado5_mmpd, esperado5_mmpd)}")
println("-" * 20)


println("=============================================")
println("=== Pruebas para multMatriz y multMatrizPar ===")
println("=============================================")
// multMatrizPar requiere matrices cuadradas de igual dimensión

// --- Caso 1: Multiplicación 1x1 aleatoria ---
val m1_1_mm = matrizAlAzar(1, 10)
val m1_2_mm = matrizAlAzar(1, 10)
val esperado1_mm = multMatriz(m1_1_mm, m1_2_mm)
val resultado1_mm_par = multMatrizPar(m1_1_mm, m1_2_mm)
println(s"Test 1: Multiplicación 1x1 aleatoria")
printMatriz(m1_1_mm, " Matriz1")
printMatriz(m1_2_mm, " Matriz2")
printMatriz(esperado1_mm, " Esperado (Seq)")
printMatriz(resultado1_mm_par, " Resultado Par")
println(s" Correcto Par == Esperado Seq: ${resultado1_mm_par == esperado1_mm}")
println("-" * 20)

// --- Caso 2: Multiplicación 2x2 aleatoria ---
val m2_1_mm = matrizAlAzar(2, 5)
val m2_2_mm = matrizAlAzar(2, 5)
val esperado2_mm = multMatriz(m2_1_mm, m2_2_mm)
val resultado2_mm_par = multMatrizPar(m2_1_mm, m2_2_mm)
println(s"Test 2: Multiplicación 2x2 aleatoria")
printMatriz(m2_1_mm, " Matriz1")
printMatriz(m2_2_mm, " Matriz2")
printMatriz(esperado2_mm, " Esperado (Seq)")
printMatriz(resultado2_mm_par, " Resultado Par")
println(s" Correcto Par == Esperado Seq: ${resultado2_mm_par == esperado2_mm}")
println("-" * 20)

// --- Caso 3: Multiplicación por identidad 2x2 (aleatoria * identidad) ---
val m3_1_mm = matrizAlAzar(2, 7)
val m3_2_mm_id = Vector(Vector(1, 0), Vector(0, 1)) // Identidad
val esperado3_mm = multMatriz(m3_1_mm, m3_2_mm_id) // Debería ser m3_1_mm
val resultado3_mm_par = multMatrizPar(m3_1_mm, m3_2_mm_id)
println(s"Test 3: Multiplicación por identidad 2x2")
printMatriz(m3_1_mm, " Matriz1")
printMatriz(m3_2_mm_id, " Matriz2 (Identidad)")
printMatriz(esperado3_mm, " Esperado (Seq)")
printMatriz(resultado3_mm_par, " Resultado Par")
println(s" Correcto Par == Esperado Seq: ${resultado3_mm_par == esperado3_mm}")
println("-" * 20)

// --- Caso 4: Multiplicación con matriz de ceros 2x2 ---
val m4_1_mm = matrizAlAzar(2, 6)
val m4_2_mm_zeros = matrizAlAzar(2, 1) // Matriz de ceros
val esperado4_mm = multMatriz(m4_1_mm, m4_2_mm_zeros) // Debería ser matriz de ceros
val resultado4_mm_par = multMatrizPar(m4_1_mm, m4_2_mm_zeros)
println(s"Test 4: Multiplicación con matriz de ceros 2x2")
printMatriz(m4_1_mm, " Matriz1")
printMatriz(m4_2_mm_zeros, " Matriz2 (Ceros)")
printMatriz(esperado4_mm, " Esperado (Seq)")
printMatriz(resultado4_mm_par, " Resultado Par")
println(s" Correcto Par == Esperado Seq: ${resultado4_mm_par == esperado4_mm}")
println("-" * 20)

// --- Caso 5: Multiplicación 4x4 aleatoria ---
val dim5_mm = 4
val m5_1_mm_r = matrizAlAzar(dim5_mm, 3)
val m5_2_mm_r = matrizAlAzar(dim5_mm, 3)
val esperado5_mm = multMatriz(m5_1_mm_r, m5_2_mm_r)
val resultado5_mm_par = multMatrizPar(m5_1_mm_r, m5_2_mm_r)
println(s"Test 5: Multiplicación ${dim5_mm}x${dim5_mm} (aleatorias)")
printMatriz(m5_1_mm_r, " Matriz1 (Aleatoria)")
printMatriz(m5_2_mm_r, " Matriz2 (Aleatoria)")
printMatriz(esperado5_mm, " Esperado (Seq)")
printMatriz(resultado5_mm_par, " Resultado Par")
println(s" Correcto Par == Esperado Seq: ${resultado5_mm_par == esperado5_mm}")
println("-" * 20)


println("=====================================================")
println("=== Pruebas para multMatrizRec y multMatrizRecPar ===")
println("=====================================================")
// Requieren matrices cuadradas y dimensión potencia de 2.

// --- Caso 1: Matriz 1x1 aleatoria ---
val m1_1_rec = matrizAlAzar(1, 15)
val m1_2_rec = matrizAlAzar(1, 15)
val esperado1_rec = multMatrizRec(m1_1_rec, m1_2_rec)
val resultado1_rec_par = multMatrizRecPar(m1_1_rec, m1_2_rec)
println(s"Test 1: Matriz 1x1 aleatoria (Rec)")
printMatriz(m1_1_rec, " Matriz1")
printMatriz(m1_2_rec, " Matriz2")
printMatriz(esperado1_rec, " Esperado (Seq Rec)")
printMatriz(resultado1_rec_par, " Resultado Par Rec")
println(s" Correcto Par == Esperado Seq: ${resultado1_rec_par == esperado1_rec}")
println("-" * 20)

// --- Caso 2: Matrices 2x2 aleatorias ---
val m2_1_rec = matrizAlAzar(2, 8)
val m2_2_rec = matrizAlAzar(2, 8)
val esperado2_rec = multMatrizRec(m2_1_rec, m2_2_rec)
val resultado2_rec_par = multMatrizRecPar(m2_1_rec, m2_2_rec)
println(s"Test 2: Matrices 2x2 aleatorias (Rec)")
printMatriz(m2_1_rec, " Matriz1")
printMatriz(m2_2_rec, " Matriz2")
printMatriz(esperado2_rec, " Esperado (Seq Rec)")
printMatriz(resultado2_rec_par, " Resultado Par Rec")
println(s" Correcto Par == Esperado Seq: ${resultado2_rec_par == esperado2_rec}")
println("-" * 20)

// --- Caso 3: Multiplicación por identidad 2x2 (aleatoria * identidad, Rec) ---
val m3_1_rec = matrizAlAzar(2, 9)
val m3_2_rec_id = Vector(Vector(1, 0), Vector(0, 1)) // Identidad
val esperado3_rec = multMatrizRec(m3_1_rec, m3_2_rec_id)
val resultado3_rec_par = multMatrizRecPar(m3_1_rec, m3_2_rec_id)
println(s"Test 3: Multiplicación por identidad 2x2 (Rec)")
printMatriz(m3_1_rec, " Matriz1")
printMatriz(m3_2_rec_id, " Matriz2 (Identidad)")
printMatriz(esperado3_rec, " Esperado (Seq Rec)")
printMatriz(resultado3_rec_par, " Resultado Par Rec")
println(s" Correcto Par == Esperado Seq: ${resultado3_rec_par == esperado3_rec}")
println("-" * 20)

// --- Caso 4: Matrices 4x4 aleatorias (Rec) ---
val dim4_rec = 4
val m4_1_rec_r = matrizAlAzar(dim4_rec, 3)
val m4_2_rec_r = matrizAlAzar(dim4_rec, 3)
val esperado4_rec = multMatrizRec(m4_1_rec_r, m4_2_rec_r)
val resultado4_rec_par = multMatrizRecPar(m4_1_rec_r, m4_2_rec_r)
println(s"Test 4: Matrices ${dim4_rec}x${dim4_rec} (aleatorias, Rec)")
printMatriz(m4_1_rec_r, " Matriz1 (Aleatoria)")
printMatriz(m4_2_rec_r, " Matriz2 (Aleatoria)")
printMatriz(esperado4_rec, " Esperado (Seq Rec)")
printMatriz(resultado4_rec_par, " Resultado Par Rec")
println(s" Correcto Par == Esperado Seq: ${resultado4_rec_par == esperado4_rec}")
println("-" * 20)

// --- Caso 5: Matrices 2x2 con una matriz de ceros (Rec) ---
val m5_1_rec = matrizAlAzar(2, 7)
val m5_2_rec_zeros = matrizAlAzar(2, 1) // Matriz de ceros
val esperado5_rec = multMatrizRec(m5_1_rec, m5_2_rec_zeros)
val resultado5_rec_par = multMatrizRecPar(m5_1_rec, m5_2_rec_zeros)
println(s"Test 5: Matrices 2x2 con una matriz de ceros (Rec)")
printMatriz(m5_1_rec, " Matriz1")
printMatriz(m5_2_rec_zeros, " Matriz2 (Ceros)")
printMatriz(esperado5_rec, " Esperado (Seq Rec)")
printMatriz(resultado5_rec_par, " Resultado Par Rec")
println(s" Correcto Par == Esperado Seq: ${resultado5_rec_par == esperado5_rec}")
println("-" * 20)


println("=====================================================")
println("=== Pruebas para multStrassen y multStrassenPar ===")
println("=====================================================")
// Requieren matrices cuadradas y dimensión potencia de 2.

// --- Caso 1: Matriz 1x1 aleatoria (Strassen) ---
val m1_1_str = matrizAlAzar(1, 12)
val m1_2_str = matrizAlAzar(1, 12)
val esperado1_str = multStrassen(m1_1_str, m1_2_str)
val resultado1_str_par = multStrassenPar(m1_1_str, m1_2_str)
println(s"Test 1: Matriz 1x1 aleatoria (Strassen)")
printMatriz(m1_1_str, " Matriz1")
printMatriz(m1_2_str, " Matriz2")
printMatriz(esperado1_str, " Esperado (Seq Strassen)")
printMatriz(resultado1_str_par, " Resultado Par Strassen")
println(s" Correcto Par == Esperado Seq: ${resultado1_str_par == esperado1_str}")
println("-" * 20)

// --- Caso 2: Matrices 2x2 aleatorias (Strassen) ---
val m2_1_str = matrizAlAzar(2, 6)
val m2_2_str = matrizAlAzar(2, 6)
val esperado2_str = multStrassen(m2_1_str, m2_2_str)
val resultado2_str_par = multStrassenPar(m2_1_str, m2_2_str)
println(s"Test 2: Matrices 2x2 aleatorias (Strassen)")
printMatriz(m2_1_str, " Matriz1")
printMatriz(m2_2_str, " Matriz2")
printMatriz(esperado2_str, " Esperado (Seq Strassen)")
printMatriz(resultado2_str_par, " Resultado Par Strassen")
println(s" Correcto Par == Esperado Seq: ${resultado2_str_par == esperado2_str}")
println("-" * 20)

// --- Caso 3: Multiplicación por identidad 2x2 (aleatoria * identidad, Strassen) ---
val m3_1_str = matrizAlAzar(2, 8)
val m3_2_str_id = Vector(Vector(1, 0), Vector(0, 1)) // Identidad
val esperado3_str = multStrassen(m3_1_str, m3_2_str_id)
val resultado3_str_par = multStrassenPar(m3_1_str, m3_2_str_id)
println(s"Test 3: Multiplicación por identidad 2x2 (Strassen)")
printMatriz(m3_1_str, " Matriz1")
printMatriz(m3_2_str_id, " Matriz2 (Identidad)")
printMatriz(esperado3_str, " Esperado (Seq Strassen)")
printMatriz(resultado3_str_par, " Resultado Par Strassen")
println(s" Correcto Par == Esperado Seq: ${resultado3_str_par == esperado3_str}")
println("-" * 20)

// --- Caso 4: Matrices 4x4 aleatorias (Strassen) ---
val dim4_str = 4
val m4_1_str_r = matrizAlAzar(dim4_str, 2)
val m4_2_str_r = matrizAlAzar(dim4_str, 2)
val esperado4_str = multStrassen(m4_1_str_r, m4_2_str_r)
val resultado4_str_par = multStrassenPar(m4_1_str_r, m4_2_str_r)
println(s"Test 4: Matrices ${dim4_str}x${dim4_str} (aleatorias, Strassen)")
printMatriz(m4_1_str_r, " Matriz1 (Aleatoria)")
printMatriz(m4_2_str_r, " Matriz2 (Aleatoria)")
printMatriz(esperado4_str, " Esperado (Seq Strassen)")
printMatriz(resultado4_str_par, " Resultado Par Strassen")
println(s" Correcto Par == Esperado Seq: ${resultado4_str_par == esperado4_str}")
println("-" * 20)

// --- Caso 5: Matrices 2x2 con una matriz de ceros (Strassen) ---
val m5_1_str = matrizAlAzar(2, 9)
val m5_2_str_zeros = matrizAlAzar(2, 1) // Matriz de ceros
val esperado5_str = multStrassen(m5_1_str, m5_2_str_zeros)
val resultado5_str_par = multStrassenPar(m5_1_str, m5_2_str_zeros)
println(s"Test 5: Matrices 2x2 con una matriz de ceros (Strassen)")
printMatriz(m5_1_str, " Matriz1")
printMatriz(m5_2_str_zeros, " Matriz2 (Ceros)")
printMatriz(esperado5_str, " Esperado (Seq Strassen)")
printMatriz(resultado5_str_par, " Resultado Par Strassen")
println(s" Correcto Par == Esperado Seq: ${resultado5_str_par == esperado5_str}")
println("-" * 20)


println("-------------------------------------------------")
println("--- Fin de las Pruebas ---")
println("-------------------------------------------------")


// ========================================================================
// PARTE 2: PRUEBAS DE RENDIMIENTO (BENCHMARKING) PRELIMINARES
// ========================================================================
// NOTA: Ejecutar benchmarks en un worksheet puede dar resultados menos
//       precisos que en una aplicación compilada (`sbt run`).
//       Estos son resultados preliminares para el informe.
//       ¡La ejecución puede tardar bastante!
// ========================================================================
println("\n--- Pruebas de Rendimiento (Benchmark) ---")
println("ADVERTENCIA: La ejecución puede tardar varios minutos...")

// --- Benchmark Producto Punto ---
println("\n=== Benchmark: Producto Punto ===")
val vectorSizesBench = List(10000, 100000, 1000000, 5000000) // Tamaños para benchmark
println(f"${"Tamaño (n)"}%15s | ${"T. Seq (ms)"}%15s | ${"T. ParD (ms)"}%15s | ${"Speedup"}%10s")
println("-" * 65)
for (n <- vectorSizesBench) {
  println(s"Benchmarking Producto Punto n=$n...")
  // La función compararProdPunto genera los vectores internamente
  val (tSeq, tParD, speedup) = compararProdPunto(n)
  println(f"$n%15d | $tSeq%15.4f | $tParD%15.4f | $speedup%9.2f x")
}

// --- Benchmark Multiplicación Matrices ---
println("\n=== Benchmark: Multiplicación Matrices ===")
// Tamaños potencia de 2.
val matrixSizesBench = List(16, 32, 64, 128, 256)

// --- Bloque 1: Comparaciones Secuencial vs Paralela para cada algoritmo ---

// Comparación 1.1: Estándar Seq vs Par
println("\n--- Comparación: multMatriz vs multMatrizPar ---")
println(f"${"Dimensión (n)"}%15s | ${"T. Seq (ms)"}%15s | ${"T. Par (ms)"}%15s | ${"Speedup"}%10s")
println("-" * 65)
for (n <- matrixSizesBench) {
  println(s"Benchmarking multMatriz vs multMatrizPar n=$n...")
  val m1 = matrizAlAzar(n, 2)
  val m2 = matrizAlAzar(n, 2)
  val (tSeq, tPar, speedup) = compararAlgoritmos(multMatriz _, multMatrizPar _)(m1, m2)
  println(f"$n%15d | $tSeq%15.4f | $tPar%15.4f | $speedup%9.2f x")
}

// Comparación 1.2: Recursiva Seq vs Par
println("\n--- Comparación: multMatrizRec vs multMatrizRecPar ---")
println(f"${"Dimensión (n)"}%15s | ${"T. Seq (ms)"}%15s | ${"T. Par (ms)"}%15s | ${"Speedup"}%10s")
println("-" * 65)
for (n <- matrixSizesBench) {
  println(s"Benchmarking multMatrizRec vs multMatrizRecPar n=$n...")
  val m1 = matrizAlAzar(n, 2)
  val m2 = matrizAlAzar(n, 2)
  val (tSeq, tPar, speedup) = compararAlgoritmos(multMatrizRec _, multMatrizRecPar _)(m1, m2)
  println(f"$n%15d | $tSeq%15.4f | $tPar%15.4f | $speedup%9.2f x")
}

// Comparación 1.3: Strassen Seq vs Par
println("\n--- Comparación: multStrassen vs multStrassenPar ---")
println(f"${"Dimensión (n)"}%15s | ${"T. Seq (ms)"}%15s | ${"T. Par (ms)"}%15s | ${"Speedup"}%10s")
println("-" * 65)
for (n <- matrixSizesBench) {
  println(s"Benchmarking multStrassen vs multStrassenPar n=$n...")
  val m1 = matrizAlAzar(n, 2)
  val m2 = matrizAlAzar(n, 2)
  val (tSeq, tPar, speedup) = compararAlgoritmos(multStrassen _, multStrassenPar _)(m1, m2)
  println(f"$n%15d | $tSeq%15.4f | $tPar%15.4f | $speedup%9.2f x")
}

// --- Bloque 2: Comparaciones entre algoritmos SECUENCIALES ---

// Comparación 2.1: Estándar Secuencial vs Recursiva Secuencial
println("\n--- Comparación: multMatriz (Seq) vs multMatrizRec (Seq) ---")
println(f"${"Dimensión (n)"}%15s | ${"T. Std (ms)"}%15s | ${"T. Rec (ms)"}%15s | ${"Ratio Std/Rec"}%15s")
println("-" * 70)
for (n <- matrixSizesBench) {
  println(s"Benchmarking multMatriz (Seq) vs multMatrizRec (Seq) n=$n...")
  val m1 = matrizAlAzar(n, 2)
  val m2 = matrizAlAzar(n, 2)
  val (tStd, tRec, ratio) = compararAlgoritmos(multMatriz _, multMatrizRec _)(m1, m2)
  println(f"$n%15d | $tStd%15.4f | $tRec%15.4f | $ratio%14.2f x")
}

// Comparación 2.2: Estándar Secuencial vs Strassen Secuencial
println("\n--- Comparación: multMatriz (Seq) vs multStrassen (Seq) ---")
println(f"${"Dimensión (n)"}%15s | ${"T. Std (ms)"}%15s | ${"T. Strass(ms)"}%15s | ${"Ratio Std/Strass"}%18s")
println("-" * 73)
for (n <- matrixSizesBench) {
  println(s"Benchmarking multMatriz (Seq) vs multStrassen (Seq) n=$n...")
  val m1 = matrizAlAzar(n, 2)
  val m2 = matrizAlAzar(n, 2)
  val (tStd, tStrassen, ratio) = compararAlgoritmos(multMatriz _, multStrassen _)(m1, m2)
  println(f"$n%15d | $tStd%15.4f | $tStrassen%15.4f | $ratio%17.2f x")
}

// Comparación 2.3: Recursiva Secuencial vs Strassen Secuencial
println("\n--- Comparación: multMatrizRec (Seq) vs multStrassen (Seq) ---")
println(f"${"Dimensión (n)"}%15s | ${"T. Rec (ms)"}%15s | ${"T. Strass(ms)"}%15s | ${"Ratio Rec/Strass"}%18s")
println("-" * 73)
for (n <- matrixSizesBench) {
  println(s"Benchmarking multMatrizRec (Seq) vs multStrassen (Seq) n=$n...")
  val m1 = matrizAlAzar(n, 2)
  val m2 = matrizAlAzar(n, 2)
  val (tRec, tStrassen, ratio) = compararAlgoritmos(multMatrizRec _, multStrassen _)(m1, m2)
  println(f"$n%15d | $tRec%15.4f | $tStrassen%15.4f | $ratio%17.2f x")
}

// --- Bloque 3: Comparaciones entre algoritmos PARALELOS (Task) ---

// Comparación 3.1: Estándar Paralela vs Recursiva Paralela
println("\n--- Comparación: multMatrizPar (Par) vs multMatrizRecPar (Par) ---")
println(f"${"Dimensión (n)"}%15s | ${"T. StdPar(ms)"}%15s | ${"T. RecPar(ms)"}%15s | ${"Ratio StdPar/RecPar"}%20s")
println("-" * 78)
for (n <- matrixSizesBench) {
  println(s"Benchmarking multMatrizPar vs multMatrizRecPar n=$n...")
  val m1 = matrizAlAzar(n, 2)
  val m2 = matrizAlAzar(n, 2)
  val (tStdPar, tRecPar, ratio) = compararAlgoritmos(multMatrizPar _, multMatrizRecPar _)(m1, m2)
  println(f"$n%15d | $tStdPar%15.4f | $tRecPar%15.4f | $ratio%19.2f x")
}

// Comparación 3.2: Estándar Paralela vs Strassen Paralela
println("\n--- Comparación: multMatrizPar (Par) vs multStrassenPar (Par) ---")
println(f"${"Dimensión (n)"}%15s | ${"T. StdPar(ms)"}%15s | ${"T. StrassP(ms)"}%15s | ${"Ratio StdPar/StrassP"}%22s")
println("-" * 80)
for (n <- matrixSizesBench) {
  println(s"Benchmarking multMatrizPar vs multStrassenPar n=$n...")
  val m1 = matrizAlAzar(n, 2)
  val m2 = matrizAlAzar(n, 2)
  val (tStdPar, tStrassenPar, ratio) = compararAlgoritmos(multMatrizPar _, multStrassenPar _)(m1, m2)
  println(f"$n%15d | $tStdPar%15.4f | $tStrassenPar%15.4f | $ratio%21.2f x")
}

// Comparación 3.3: Recursiva Paralela vs Strassen Paralela
println("\n--- Comparación: multMatrizRecPar (Par) vs multStrassenPar (Par) ---")
println(f"${"Dimensión (n)"}%15s | ${"T. RecPar(ms)"}%15s | ${"T. StrassP(ms)"}%15s | ${"Ratio RecPar/StrassP"}%22s")
println("-" * 80)
for (n <- matrixSizesBench) {
  println(s"Benchmarking multMatrizRecPar vs multStrassenPar n=$n...")
  val m1 = matrizAlAzar(n, 2)
  val m2 = matrizAlAzar(n, 2)
  val (tRecPar, tStrassenPar, ratio) = compararAlgoritmos(multMatrizRecPar _, multStrassenPar _)(m1, m2)
  println(f"$n%15d | $tRecPar%15.4f | $tStrassenPar%15.4f | $ratio%21.2f x")
}

println("\n--- Fin Pruebas de Rendimiento ---")