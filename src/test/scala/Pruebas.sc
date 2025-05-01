// Importa todo del paquete Matrices (incluye el tipo Matriz y las funciones)
import Matrices._
// Importa el conversor para poder usar .par en colecciones estándar
import scala.collection.parallel.CollectionConverters._
// Podrías importar Benchmark si quieres llamar a compararProdPunto aquí,
// pero usualmente las pruebas de funcionamiento se hacen separadas del benchmarking.
// import Benchmark._

println("--- Pruebas Taller 5: Multiplicación de Matrices y Producto Punto ---")

// --- Crear Datos de Prueba ---
// Matrices pequeñas (potencia de 2 para algoritmos recursivos/Strassen)
val n_small = 4 // Dimensión pequeña (4x4)
val vals_limit = 3 // Valores pequeños (0, 1, 2) para facilitar la inspección manual

val m1_small = matrizAlAzar(n_small, vals_limit)
val m2_small = matrizAlAzar(n_small, vals_limit)

println(s"\nMatriz m1 ($n_small x $n_small):")
m1_small.foreach(row => println(row.mkString("\t")))
println(s"\nMatriz m2 ($n_small x $n_small):")
m2_small.foreach(row => println(row.mkString("\t")))

// Vectores pequeños
val vec_len_small = 10
val v1_small = vectorAlAzar(vec_len_small, vals_limit)
val v2_small = vectorAlAzar(vec_len_small, vals_limit)

println(s"\nVector v1 (longitud $vec_len_small): $v1_small")
println(s"Vector v2 (longitud $vec_len_small): $v2_small")


// --- Pruebas de Multiplicación de Matrices ---

println("\n--- Probando Multiplicación Estándar ---")
val res_multMatriz = multMatriz(m1_small, m2_small)
println("Resultado multMatriz (Seq):")
res_multMatriz.foreach(row => println(row.mkString("\t")))

val res_multMatrizPar = multMatrizPar(m1_small, m2_small)
println("\nResultado multMatrizPar (Par):")
res_multMatrizPar.foreach(row => println(row.mkString("\t")))
// Verificar si son iguales (deberían serlo si no hay errores)
println(s"multMatriz == multMatrizPar: ${res_multMatriz == res_multMatrizPar}")


println("\n--- Probando Multiplicación Recursiva ---")
val res_multMatrizRec = multMatrizRec(m1_small, m2_small)
println("Resultado multMatrizRec (Seq):")
res_multMatrizRec.foreach(row => println(row.mkString("\t")))
// Verificar si es igual al estándar
println(s"multMatriz == multMatrizRec: ${res_multMatriz == res_multMatrizRec}")


// Para multMatrizRecPar y multStrassenPar, puedes ajustar el umbral si es necesario
val umbral_rec = 2 // Umbral bajo para forzar paralelismo incluso en matriz 4x4
val res_multMatrizRecPar = multMatrizRecPar(m1_small, m2_small, umbral_rec)
println(s"\nResultado multMatrizRecPar (Par, umbral=$umbral_rec):")
res_multMatrizRecPar.foreach(row => println(row.mkString("\t")))
// Verificar si es igual al estándar
println(s"multMatriz == multMatrizRecPar: ${res_multMatriz == res_multMatrizRecPar}")


println("\n--- Probando Multiplicación Strassen ---")
val res_multStrassen = multStrassen(m1_small, m2_small)
println("Resultado multStrassen (Seq):")
res_multStrassen.foreach(row => println(row.mkString("\t")))
// Verificar si es igual al estándar
println(s"multMatriz == multStrassen: ${res_multMatriz == res_multStrassen}")


// Para multMatrizRecPar y multStrassenPar, puedes ajustar el umbral si es necesario
val umbral_strassen = 2 // Umbral bajo para forzar paralelismo
val res_multStrassenPar = multStrassenPar(m1_small, m2_small, umbral_strassen)
println(s"\nResultado multStrassenPar (Par, umbral=$umbral_strassen):")
res_multStrassenPar.foreach(row => println(row.mkString("\t")))
// Verificar si es igual al estándar
println(s"multMatriz == multStrassenPar: ${res_multMatriz == res_multStrassenPar}")


// --- Pruebas de Producto Punto ---

println("\n--- Probando Producto Punto ---")
val res_prodPunto = prodPunto(v1_small, v2_small)
println(s"Resultado prodPunto (Seq): $res_prodPunto")

// Crear versiones ParVector para prodPuntoParD
val v1_par = v1_small.par
val v2_par = v2_small.par
val res_prodPuntoParD = prodPuntoParD(v1_par, v2_par)
println(s"Resultado prodPuntoParD (Par): $res_prodPuntoParD")
// Verificar si son iguales
println(s"prodPunto == prodPuntoParD: ${res_prodPunto == res_prodPuntoParD}")


// --- Prueba Opcional: Multiplicación con Paralelismo de Datos ---
// (Necesaria para Benchmark.compararMultMatriz)
println("\n--- Probando Multiplicación (Paralelismo Datos Puros) ---")
val m1_parD = transformToParD(m1_small)
val m2_parD = transformToParD(m2_small)
val res_multMatrizParD = multMatrizParD(m1_parD, m2_parD)
// Convertir resultado ParVector a Vector para comparación fácil
val res_multMatrizParD_seq: Matriz = res_multMatrizParD.map(_.seq).seq
println("Resultado multMatrizParD (Par Data):")
res_multMatrizParD_seq.foreach(row => println(row.mkString("\t")))
// Verificar si es igual al estándar
println(s"multMatriz == multMatrizParD: ${res_multMatriz == res_multMatrizParD_seq}")


println("\n--- Fin de las Pruebas ---")