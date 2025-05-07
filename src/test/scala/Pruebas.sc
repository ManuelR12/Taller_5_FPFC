import Matrices._
import Benchmark._ // ¡Importante importar el paquete Benchmark!
import scala.collection.parallel.CollectionConverters._ // Para .par en prodPuntoParD

// ========================================================================
// PARTE 1: PRUEBAS FUNCIONALES BÁSICAS (Verificar que compila y da resultados)
// ========================================================================
println("--- Pruebas Funcionales Básicas Taller 5 ---")

// --- Datos de Prueba Pequeños ---
val n_small_func = 4
val vals_limit_func = 3
val m1_func = matrizAlAzar(n_small_func, vals_limit_func)
val m2_func = matrizAlAzar(n_small_func, vals_limit_func)
val v1_func = vectorAlAzar(10, vals_limit_func)
val v2_func = vectorAlAzar(10, vals_limit_func)

println("\nMatrices Pequeñas para Pruebas Funcionales:")
println("m1:"); m1_func.foreach(r => println(r.mkString("\t")))
println("m2:"); m2_func.foreach(r => println(r.mkString("\t")))
println(s"\nVectores Pequeños: v1=$v1_func, v2=$v2_func")

// --- Ejecución Simple de Cada Función ---
println("\nEjecutando funciones (resultado puede variar por aleatoriedad):")
val res_multMatriz_func = multMatriz(m1_func, m2_func)
println(s"multMatriz: OK (devuelve ${res_multMatriz_func.length}x${res_multMatriz_func.head.length})")
val res_multMatrizPar_func = multMatrizPar(m1_func, m2_func)
println(s"multMatrizPar: OK (devuelve ${res_multMatrizPar_func.length}x${res_multMatrizPar_func.head.length})")
val res_multMatrizRec_func = multMatrizRec(m1_func, m2_func)
println(s"multMatrizRec: OK (devuelve ${res_multMatrizRec_func.length}x${res_multMatrizRec_func.head.length})")
val res_multMatrizRecPar_func = multMatrizRecPar(m1_func, m2_func) // Sin umbral
println(s"multMatrizRecPar: OK (devuelve ${res_multMatrizRecPar_func.length}x${res_multMatrizRecPar_func.head.length})")
val res_multStrassen_func = multStrassen(m1_func, m2_func)
println(s"multStrassen: OK (devuelve ${res_multStrassen_func.length}x${res_multStrassen_func.head.length})")
val res_multStrassenPar_func = multStrassenPar(m1_func, m2_func) // Sin umbral
println(s"multStrassenPar: OK (devuelve ${res_multStrassenPar_func.length}x${res_multStrassenPar_func.head.length})")
val res_prodPunto_func = prodPunto(v1_func, v2_func)
println(s"prodPunto: OK (devuelve $res_prodPunto_func)")
val res_prodPuntoParD_func = prodPuntoParD(v1_func.par, v2_func.par)
println(s"prodPuntoParD: OK (devuelve $res_prodPuntoParD_func)")

// Verificación rápida de igualdad (opcional, útil si las entradas no fueran aleatorias)
// println(s"Func Check multMatriz == multMatrizPar: ${res_multMatriz_func == res_multMatrizPar_func}")
// println(s"Func Check multMatriz == multMatrizRec: ${res_multMatriz_func == res_multMatrizRec_func}")
// println(s"Func Check multMatriz == multMatrizRecPar: ${res_multMatriz_func == res_multMatrizRecPar_func}")
// println(s"Func Check multMatriz == multStrassen: ${res_multMatriz_func == res_multStrassen_func}")
// println(s"Func Check multMatriz == multStrassenPar: ${res_multMatriz_func == res_multStrassenPar_func}")
// println(s"Func Check prodPunto == prodPuntoParD: ${res_prodPunto_func == res_prodPuntoParD_func}")

println("\n--- Fin Pruebas Funcionales ---")


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
// Tamaños potencia de 2. Ir aumentando con cuidado, 256 o 512 pueden ser lentos en worksheet.
val matrixSizesBench = List(16, 32, 64, 128, 256)

// Comparación 1: Estándar Seq vs Par
println("\n--- Comparación: multMatriz vs multMatrizPar ---")
println(f"${"Dimensión (n)"}%15s | ${"T. Seq (ms)"}%15s | ${"T. Par (ms)"}%15s | ${"Speedup"}%10s")
println("-" * 65)
for (n <- matrixSizesBench) {
  println(s"Benchmarking multMatriz vs multMatrizPar n=$n...")
  val m1 = matrizAlAzar(n, 2)
  val m2 = matrizAlAzar(n, 2)
  val (tSeq, tPar, speedup) = compararAlgoritmos(multMatriz, multMatrizPar)(m1, m2)
  println(f"$n%15d | $tSeq%15.4f | $tPar%15.4f | $speedup%9.2f x")
}

// Comparación 2: Recursiva Seq vs Par (sin umbral)
println("\n--- Comparación: multMatrizRec vs multMatrizRecPar (sin umbral) ---")
println(f"${"Dimensión (n)"}%15s | ${"T. Seq (ms)"}%15s | ${"T. Par (ms)"}%15s | ${"Speedup"}%10s")
println("-" * 65)
for (n <- matrixSizesBench) {
  println(s"Benchmarking multMatrizRec vs multMatrizRecPar n=$n...")
  val m1 = matrizAlAzar(n, 2)
  val m2 = matrizAlAzar(n, 2)
  val (tSeq, tPar, speedup) = compararAlgoritmos(multMatrizRec, multMatrizRecPar)(m1, m2)
  println(f"$n%15d | $tSeq%15.4f | $tPar%15.4f | $speedup%9.2f x") // Esperar speedup < 1
}

// Comparación 3: Strassen Seq vs Par (sin umbral)
println("\n--- Comparación: multStrassen vs multStrassenPar (sin umbral) ---")
println(f"${"Dimensión (n)"}%15s | ${"T. Seq (ms)"}%15s | ${"T. Par (ms)"}%15s | ${"Speedup"}%10s")
println("-" * 65)
for (n <- matrixSizesBench) {
  println(s"Benchmarking multStrassen vs multStrassenPar n=$n...")
  val m1 = matrizAlAzar(n, 2)
  val m2 = matrizAlAzar(n, 2)
  val (tSeq, tPar, speedup) = compararAlgoritmos(multStrassen, multStrassenPar)(m1, m2)
  println(f"$n%15d | $tSeq%15.4f | $tPar%15.4f | $speedup%9.2f x") // Esperar speedup < 1
}


println("\n--- Fin Pruebas de Rendimiento ---")