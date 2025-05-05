// fichero: pruebas.sc

import Benchmark._
import Matrices._
import org.scalameter._ // Necesario si usaras 'config' directamente aquí, aunque ya está encapsulado.

println("--- Inicio de Benchmarks ---")

// --- Configuración ---
// Define el tamaño de las matrices a probar.
// ¡IMPORTANTE! Para multMatrizRec, multMatrizRecPar, multStrassen, multStrassenPar
// 'n' DEBE ser una potencia de 2.
val n: Int = 128 // Ejemplo: 64, 128, 256. Ajusta según sea necesario.

// Define el tamaño de los vectores para el producto punto.
val vectorSize: Int = 1000000 // Un tamaño grande para ver mejor el efecto del paralelismo.

println(s"Configuración:")
println(s" - Tamaño de Matriz (n x n): $n x $n")
println(s" - Tamaño de Vector (producto punto): $vectorSize")
// El rango de valores aleatorios se define dentro de matrizAlAzar/vectorAlAzar en el paquete Matrices (actualmente usa 'vals = 2').

// --- Generación de Datos para compararAlgoritmos ---
// Generamos un par de matrices aquí para usarlas consistentemente en las comparaciones
// que utilizan `compararAlgoritmos`. Las funciones `compararMultMatriz` y
// `compararProdPunto` generan sus propios datos internamente.
println(s"\nGenerando matrices aleatorias de ${n}x${n} para comparaciones...")
// Asegúrate de que 'n' sea potencia de 2 si vas a usar Rec o Strassen.
val m1 = matrizAlAzar(n, 10) // Usamos un rango pequeño para los valores, e.g., 10
val m2 = matrizAlAzar(n, 10)
println("Matrices generadas.")


// --- Ejecución de Benchmarks ---

println("\n--- 1. Comparación de Multiplicación Estándar (Secuencial vs Paralela Task) ---")
try {
  // CORREGIDO: Añadido '_' a los nombres de los métodos
  val (tStdSec, tStdParT, speedupStdT) = compararAlgoritmos(multMatriz _, multMatrizPar _)(m1, m2)
  println(f"  Tiempo Estándar Secuencial : $tStdSec%.4f ms")
  println(f"  Tiempo Estándar Paralela (Task): $tStdParT%.4f ms")
  println(f"  Speedup (Sec/ParTask)       : $speedupStdT%.2fx")
} catch {
  case e: Throwable => println(s"  ERROR ejecutando benchmark estándar: ${e.getMessage}")
}

println("\n--- 2. Comparación de Multiplicación Recursiva (Secuencial vs Paralela Task) ---")
if ((n & (n - 1)) == 0 && n > 0) { // Verifica si n es potencia de 2
  try {
    // CORREGIDO: Añadido '_' a los nombres de los métodos
    val (tRecSec, tRecPar, speedupRec) = compararAlgoritmos(multMatrizRec _, multMatrizRecPar _)(m1, m2)
    println(f"  Tiempo Recursiva Secuencial : $tRecSec%.4f ms")
    println(f"  Tiempo Recursiva Paralela (Task): $tRecPar%.4f ms")
    println(f"  Speedup (Sec/ParTask)         : $speedupRec%.2fx")
  } catch {
    case e: Throwable => println(s"  ERROR ejecutando benchmark recursivo: ${e.getMessage}")
  }
} else {
  println(s"  **Omitido**: El tamaño de la matriz n=$n no es potencia de 2, requerido para algoritmos recursivos.")
}


println("\n--- 3. Comparación de Multiplicación Strassen (Secuencial vs Paralela Task) ---")
if ((n & (n - 1)) == 0 && n > 0) { // Verifica si n es potencia de 2
  try {
    // CORREGIDO: Añadido '_' a los nombres de los métodos
    val (tStrSec, tStrPar, speedupStr) = compararAlgoritmos(multStrassen _, multStrassenPar _)(m1, m2)
    println(f"  Tiempo Strassen Secuencial  : $tStrSec%.4f ms")
    println(f"  Tiempo Strassen Paralela (Task): $tStrPar%.4f ms")
    println(f"  Speedup (Sec/ParTask)         : $speedupStr%.2fx")
  } catch {
    case e: Throwable => println(s"  ERROR ejecutando benchmark Strassen: ${e.getMessage}")
  }
} else {
  println(s"  **Omitido**: El tamaño de la matriz n=$n no es potencia de 2, requerido para Strassen.")
}


println("\n--- 4. Comparación Multiplicación Estándar (Secuencial vs Paralela Datos) ---")
// Usa la función específica compararMultMatriz que genera sus propias matrices.
// No necesita '_' porque no pasa funciones como argumentos.
try {
  val (tStdSecD, tStdParD, speedupStdD) = compararMultMatriz(n)
  println(f"  Tiempo Estándar Secuencial     : $tStdSecD%.4f ms")
  println(f"  Tiempo Estándar Paralela (Datos): $tStdParD%.4f ms")
  println(f"  Speedup (Sec/ParDatos)         : $speedupStdD%.2fx")
} catch {
  case e: Throwable => println(s"  ERROR ejecutando benchmark estándar (datos): ${e.getMessage}")
}


println("\n--- 5. Comparación Producto Punto (Secuencial vs Paralelo Datos) ---")
// Usa la función específica compararProdPunto que genera sus propios vectores.
// No necesita '_' porque no pasa funciones como argumentos.
try {
  val (tPPSec, tPPParD, speedupPP) = compararProdPunto(vectorSize)
  println(f"  Tiempo Producto Punto Secuencial : $tPPSec%.4f ms")
  println(f"  Tiempo Producto Punto Paralelo (Datos): $tPPParD%.4f ms")
  println(f"  Speedup (Sec/ParDatos)           : $speedupPP%.2fx")
} catch {
  case e: Throwable => println(s"  ERROR ejecutando benchmark producto punto: ${e.getMessage}")
}


// --- Comparaciones Adicionales (Opcional) ---

println("\n--- 6. Comparación Algoritmos Secuenciales (Estándar vs Recursivo vs Strassen) ---")
if ((n & (n - 1)) == 0 && n > 0) { // Solo si n es potencia de 2
  try {
    // CORREGIDO: Añadido '_' a los nombres de los métodos
    println("  Comparando Estándar Sec vs Recursiva Sec...")
    val (tStdSec_vs_Rec, tRecSec_vs_Std, speedupStdVsRec) = compararAlgoritmos(multMatriz _, multMatrizRec _)(m1, m2)
    println(f"    Estándar Sec  : $tStdSec_vs_Rec%.4f ms")
    println(f"    Recursiva Sec : $tRecSec_vs_Std%.4f ms")
    println(f"    Aceleración (Estándar / Recursiva): $speedupStdVsRec%.2fx")

    // CORREGIDO: Añadido '_' a los nombres de los métodos
    println("\n  Comparando Estándar Sec vs Strassen Sec...")
    val (tStdSec_vs_Str, tStrSec_vs_Std, speedupStdVsStr) = compararAlgoritmos(multMatriz _, multStrassen _)(m1, m2)
    println(f"    Estándar Sec : $tStdSec_vs_Str%.4f ms")
    println(f"    Strassen Sec : $tStrSec_vs_Std%.4f ms")
    println(f"    Aceleración (Estándar / Strassen): $speedupStdVsStr%.2fx")

    // CORREGIDO: Añadido '_' a los nombres de los métodos
    println("\n  Comparando Recursiva Sec vs Strassen Sec...")
    val (tRecSec_vs_Str, tStrSec_vs_Rec, speedupRecVsStr) = compararAlgoritmos(multMatrizRec _, multStrassen _)(m1, m2)
    println(f"    Recursiva Sec: $tRecSec_vs_Str%.4f ms")
    println(f"    Strassen Sec : $tStrSec_vs_Rec%.4f ms")
    println(f"    Aceleración (Recursiva / Strassen): $speedupRecVsStr%.2fx")

  } catch {
    case e: Throwable => println(s"  ERROR ejecutando benchmarks comparativos secuenciales: ${e.getMessage}")
  }
} else {
  println(s"  **Omitido**: El tamaño de la matriz n=$n no es potencia de 2.")
}


println("\n--- Fin de Benchmarks ---")