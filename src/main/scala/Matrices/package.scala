import scala.util.Random
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.CollectionConverters._ // Necesario para .par
import common._ // Para usar parallel y task definidos por el usuario

package object Matrices {
  val random = new Random()

  type Matriz = Vector[Vector[Int]]
  type MatrizPar = ParVector[ParVector[Int]] // Tipo para Paralelismo de Datos

  /**
   * Crea una matriz cuadrada de enteros de tamaño long x long.
   * con valores aleatorios entre 0 y vals (exclusivo).
   */
  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    Vector.fill(long, long)(random.nextInt(vals))
  }

  /**
   * Crea un vector de enteros de longitud long.
   * con valores aleatorios entre 0 y vals (exclusivo).
   */
  def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
    Vector.fill(long)(random.nextInt(vals))
  }

  /**
   * Calcula la transpuesta de una matriz m (Vector[Vector[Int]]).
   */
  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    if (l == 0) Vector.empty
    else {
      val w = m(0).length
      Vector.tabulate(w, l)((i, j) => m(j)(i)) // Correcto para matrices no cuadradas también
    }
  }

  /**
   * Calcula el producto punto entre dos vectores v1 y v2.
   */
  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    require(v1.length == v2.length, "Los vectores deben tener la misma longitud para el producto punto")
    (v1 zip v2).map { case (i, j) => i * j }.sum
  }

  /**
   * Calcula el producto punto entre dos ParVectors v1 y v2 usando paralelismo de datos.
   */
  def prodPuntoParD(v1: ParVector[Int], v2: ParVector[Int]): Int = {
    require(v1.length == v2.length, "Los vectores deben tener la misma longitud para el producto punto")
    (v1 zip v2).map { case (i, j) => i * j }.sum // map y sum son paralelos en ParVector
  }

  // --- Funciones para soportar Benchmark.compararMultMatriz ---

  /**
   * Convierte una Matriz (Vector[Vector[Int]]) a MatrizPar (ParVector[ParVector[Int]]).
   */
  def transformToParD(m: Matriz): MatrizPar = {
    m.par.map(_.par)
  }

  /**
   * Calcula la transpuesta de una MatrizPar m (ParVector[ParVector[Int]]).
   */
  def transposeParD(m: MatrizPar): MatrizPar = {
    val l = m.length
    if (l == 0) ParVector.empty
    else {
      val w = m.head.length // Asume matriz no vacía y rectangular
      ParVector.tabulate(w, l)((i, j) => m(j)(i))
    }
  }

  /**
   * Multiplicación de matrices usando paralelismo de datos (ParVector).
   */
  def multMatrizParD(m1: MatrizPar, m2: MatrizPar): MatrizPar = {
    require(m1.headOption.exists(_.length == m2.length), "Dimensiones incompatibles para multiplicación")
    val m2TransPar = transposeParD(m2)
    val n = m1.length
    // Calcula filas en paralelo
    ParVector.tabulate(n, m2TransPar.length) { (i, j) =>
      prodPuntoParD(m1(i), m2TransPar(j)) // Producto punto paralelo
    }
  }


  // --- Ejercicio 1.1: Multiplicación Estándar ---

  /**
   * Ejercicio 1.1.1: Multiplicación estándar secuencial de matrices.
   */
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.headOption.exists(_.length == m2.length), "Dimensiones incompatibles para multiplicación")
    val m2trans = transpuesta(m2)
    val n = m1.length
    val p = m2trans.length // Número de columnas de m2 (o filas de m2trans)
    Vector.tabulate(n, p)((i, j) => prodPunto(m1(i), m2trans(j)))
  }

  /**
   * Ejercicio 1.1.2: Multiplicación estándar paralela (paralelismo de tareas).
   * Paraleliza el cálculo de las filas de la matriz resultado usando common.parallel o common.task.
   * Usaremos .par.map como una forma sencilla de paralelismo de tareas/datos aquí.
   */
  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.headOption.exists(_.length == m2.length), "Dimensiones incompatibles para multiplicación")
    val m2trans = transpuesta(m2)
    val n = m1.length
    val p = m2trans.length

    // Usar la colección paralela para calcular filas en paralelo (combina tareas y datos)
    (0 until n).par.map { i =>
      Vector.tabulate(p)(j => prodPunto(m1(i), m2trans(j)))
    }.toVector
  }

  // --- Ejercicio 1.2: Multiplicación Recursiva ---

  /**
   * Ejercicio 1.2.1: Extrae una submatriz cuadrada de tamaño l x l
   * comenzando en la posición (i, j) de la matriz m.
   */
  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    require(i >= 0 && i + l <= m.length && j >= 0 && j + l <= m(0).length, "Índices o dimensión inválidos para submatriz")
    Vector.tabulate(l, l)((row, col) => m(i + row)(j + col))
  }

  /**
   * Ejercicio 1.2.2: Suma dos matrices m1 y m2 de la misma dimensión.
   */
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m2.length && m1.headOption.exists(_.length == m2.headOption.map(_.length).getOrElse(-1)), "Matrices deben tener la misma dimensión para sumar")
    val n = m1.length
    val p = m1(0).length
    Vector.tabulate(n, p)((i, j) => m1(i)(j) + m2(i)(j))
  }

  /**
   * Ejercicio 1.2.3: Multiplicación recursiva secuencial de matrices.
   * Asume que las dimensiones son potencias de 2.
   */
  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m1(0).length && m2.length == m2(0).length && m1.length == m2.length, "Matrices deben ser cuadradas y de igual dimensión")
    require(m1.nonEmpty && (m1.length & (m1.length - 1)) == 0, "La dimensión de la matriz debe ser potencia de 2")

    val n = m1.length
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val l = n / 2
      val a11 = subMatriz(m1, 0, 0, l)
      val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l)
      val a22 = subMatriz(m1, l, l, l)
      val b11 = subMatriz(m2, 0, 0, l)
      val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l)
      val b22 = subMatriz(m2, l, l, l)

      val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
      val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
      val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
      val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))

      Vector.tabulate(n, n) { (i, j) =>
        if (i < l && j < l) c11(i)(j)
        else if (i < l && j >= l) c12(i)(j - l)
        else if (i >= l && j < l) c21(i - l)(j)
        else c22(i - l)(j - l)
      }
    }
  }

  /**
   * Ejercicio 1.2.4: Multiplicación recursiva paralela de matrices (paralelismo de tareas).
   * Usa 'common.task' y 'join'.
   * Incluye un umbral para cambiar a secuencial.
   */
  def multMatrizRecPar(m1: Matriz, m2: Matriz, umbral: Int = 16): Matriz = {
    require(m1.length == m1(0).length && m2.length == m2(0).length && m1.length == m2.length, "Matrices deben ser cuadradas y de igual dimensión")
    require(m1.nonEmpty && (m1.length & (m1.length - 1)) == 0, "La dimensión de la matriz debe ser potencia de 2")

    val n = m1.length
    if (n <= umbral) {
      multMatriz(m1, m2) // Base secuencial eficiente
    } else {
      val l = n / 2
      val a11 = subMatriz(m1, 0, 0, l)
      val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l)
      val a22 = subMatriz(m1, l, l, l)
      val b11 = subMatriz(m2, 0, 0, l)
      val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l)
      val b22 = subMatriz(m2, l, l, l)

      // Calcular productos intermedios en paralelo usando 'task'
      // C11 = A11*B11 + A12*B21
      val p1 = task { multMatrizRecPar(a11, b11, umbral) }
      val p2 = task { multMatrizRecPar(a12, b21, umbral) }
      // C12 = A11*B12 + A12*B22
      val p3 = task { multMatrizRecPar(a11, b12, umbral) }
      val p4 = task { multMatrizRecPar(a12, b22, umbral) }
      // C21 = A21*B11 + A22*B21
      val p5 = task { multMatrizRecPar(a21, b11, umbral) }
      val p6 = task { multMatrizRecPar(a22, b21, umbral) }
      // C22 = A21*B12 + A22*B22
      val p7 = task { multMatrizRecPar(a21, b12, umbral) }
      val p8 = task { multMatrizRecPar(a22, b22, umbral) }

      // Sincronizar y sumar (la suma es secuencial)
      val c11 = sumMatriz(p1.join(), p2.join())
      val c12 = sumMatriz(p3.join(), p4.join())
      val c21 = sumMatriz(p5.join(), p6.join())
      val c22 = sumMatriz(p7.join(), p8.join())

      // Ensamblar la matriz resultado
      Vector.tabulate(n, n) { (i, j) =>
        if (i < l && j < l) c11(i)(j)
        else if (i < l && j >= l) c12(i)(j - l)
        else if (i >= l && j < l) c21(i - l)(j)
        else c22(i - l)(j - l)
      }
    }
  }

  // --- Ejercicio 1.3: Multiplicación con Algoritmo de Strassen ---

  /**
   * Ejercicio 1.3.1: Resta dos matrices m1 y m2 de la misma dimensión.
   */
  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m2.length && m1.headOption.exists(_.length == m2.headOption.map(_.length).getOrElse(-1)), "Matrices deben tener la misma dimensión para restar")
    val n = m1.length
    val p = m1(0).length
    Vector.tabulate(n, p)((i, j) => m1(i)(j) - m2(i)(j))
  }

  /**
   * Ejercicio 1.3.2: Multiplicación secuencial con algoritmo de Strassen.
   * Asume que las dimensiones son potencias de 2.
   */
  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m1(0).length && m2.length == m2(0).length && m1.length == m2.length, "Matrices deben ser cuadradas y de igual dimensión")
    require(m1.nonEmpty && (m1.length & (m1.length - 1)) == 0, "La dimensión de la matriz debe ser potencia de 2")

    val n = m1.length
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val l = n / 2
      val a11 = subMatriz(m1, 0, 0, l)
      val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l)
      val a22 = subMatriz(m1, l, l, l)
      val b11 = subMatriz(m2, 0, 0, l)
      val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l)
      val b22 = subMatriz(m2, l, l, l)

      val s1 = restaMatriz(b12, b22)
      val s2 = sumMatriz(a11, a12)
      val s3 = sumMatriz(a21, a22)
      val s4 = restaMatriz(b21, b11)
      val s5 = sumMatriz(a11, a22)
      val s6 = sumMatriz(b11, b22)
      val s7 = restaMatriz(a12, a22)
      val s8 = sumMatriz(b21, b22)
      val s9 = restaMatriz(a11, a21)
      val s10 = sumMatriz(b11, b12)

      val p1 = multStrassen(a11, s1)
      val p2 = multStrassen(s2, b22)
      val p3 = multStrassen(s3, b11)
      val p4 = multStrassen(a22, s4)
      val p5 = multStrassen(s5, s6)
      val p6 = multStrassen(s7, s8)
      val p7 = multStrassen(s9, s10)

      val c11 = sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6)
      val c12 = sumMatriz(p1, p2)
      val c21 = sumMatriz(p3, p4)
      val c22 = restaMatriz(restaMatriz(sumMatriz(p5, p1), p3), p7)

      Vector.tabulate(n, n) { (i, j) =>
        if (i < l && j < l) c11(i)(j)
        else if (i < l && j >= l) c12(i)(j - l)
        else if (i >= l && j < l) c21(i - l)(j)
        else c22(i - l)(j - l)
      }
    }
  }

  /**
   * Ejercicio 1.3.3: Multiplicación paralela con algoritmo de Strassen (paralelismo de tareas).
   * Usa 'common.task' y 'join'.
   * Incluye un umbral.
   */
  def multStrassenPar(m1: Matriz, m2: Matriz, umbral: Int = 64): Matriz = {
    require(m1.length == m1(0).length && m2.length == m2(0).length && m1.length == m2.length, "Matrices deben ser cuadradas y de igual dimensión")
    require(m1.nonEmpty && (m1.length & (m1.length - 1)) == 0, "La dimensión de la matriz debe ser potencia de 2")

    val n = m1.length
    if (n <= umbral) {
      multMatriz(m1, m2) // Base secuencial eficiente
    } else {
      val l = n / 2
      val a11 = subMatriz(m1, 0, 0, l)
      val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l)
      val a22 = subMatriz(m1, l, l, l)
      val b11 = subMatriz(m2, 0, 0, l)
      val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l)
      val b22 = subMatriz(m2, l, l, l)

      // Las sumas/restas S son rápidas, no vale la pena paralelizar aquí
      val s1 = restaMatriz(b12, b22)
      val s2 = sumMatriz(a11, a12)
      val s3 = sumMatriz(a21, a22)
      val s4 = restaMatriz(b21, b11)
      val s5 = sumMatriz(a11, a22)
      val s6 = sumMatriz(b11, b22)
      val s7 = restaMatriz(a12, a22)
      val s8 = sumMatriz(b21, b22)
      val s9 = restaMatriz(a11, a21)
      val s10 = sumMatriz(b11, b12)

      // Calcular P1 a P7 en paralelo usando 'task'
      val p1 = task { multStrassenPar(a11, s1, umbral) }
      val p2 = task { multStrassenPar(s2, b22, umbral) }
      val p3 = task { multStrassenPar(s3, b11, umbral) }
      val p4 = task { multStrassenPar(a22, s4, umbral) }
      val p5 = task { multStrassenPar(s5, s6, umbral) }
      val p6 = task { multStrassenPar(s7, s8, umbral) }
      val p7 = task { multStrassenPar(s9, s10, umbral) }

      // Sincronizar y calcular C (las sumas/restas son secuenciales)
      val c11 = sumMatriz(restaMatriz(sumMatriz(p5.join(), p4.join()), p2.join()), p6.join())
      val c12 = sumMatriz(p1.join(), p2.join())
      val c21 = sumMatriz(p3.join(), p4.join())
      val c22 = restaMatriz(restaMatriz(sumMatriz(p5.join(), p1.join()), p3.join()), p7.join())

      // Ensamblar la matriz resultado
      Vector.tabulate(n, n) { (i, j) =>
        if (i < l && j < l) c11(i)(j)
        else if (i < l && j >= l) c12(i)(j - l)
        else if (i >= l && j < l) c21(i - l)(j)
        else c22(i - l)(j - l)
      }
    }
  }

} // Fin package object Matrices