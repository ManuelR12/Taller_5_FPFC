import scala.util.Random
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.CollectionConverters._
import java.util.concurrent.ForkJoinTask
import common._

package object Matrices {
  val random = new Random()

  type Matriz = Vector[Vector[Int]]
  type MatrizPar = ParVector[ParVector[Int]]

  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    Vector.fill(long, long)(random.nextInt(vals))
  }

  def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
    Vector.fill(long)(random.nextInt(vals))
  }

  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    if (l == 0) Vector.empty
    else {
      val w = m(0).length
      Vector.tabulate(w, l)((i, j) => m(j)(i))
    }
  }

  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    require(v1.length == v2.length, "Vectores deben tener misma longitud")
    (v1 zip v2).map { case (i, j) => i * j }.sum
  }

  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    require(i >= 0 && i + l <= m.length && j >= 0 && j + l <= m(0).length, "Índices/dimensión inválidos")
    Vector.tabulate(l, l)((row, col) => m(i + row)(j + col))
  }

  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m2.length && m1.headOption.exists(_.length == m2.headOption.map(_.length).getOrElse(-1)), "Matrices deben tener misma dimensión")
    val n = m1.length
    val p = m1(0).length
    Vector.tabulate(n, p)((i, j) => m1(i)(j) + m2(i)(j))
  }

  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m2.length && m1.headOption.exists(_.length == m2.headOption.map(_.length).getOrElse(-1)), "Matrices deben tener misma dimensión")
    val n = m1.length
    val p = m1(0).length
    Vector.tabulate(n, p)((i, j) => m1(i)(j) - m2(i)(j))
  }

  def prodPuntoParD(v1: ParVector[Int], v2: ParVector[Int]): Int = {
    require(v1.length == v2.length, "Vectores deben tener misma longitud")
    (v1 zip v2).map { case (i, j) => i * j }.sum
  }

  def transformToParD(m: Matriz): MatrizPar = {
    m.par.map(_.par)
  }

  def transposeParD(m: MatrizPar): MatrizPar = {
    val l = m.length
    if (l == 0) ParVector.empty
    else {
      val w = m.head.length
      ParVector.tabulate(w, l)((i, j) => m(j)(i))
    }
  }

  def multMatrizParD(m1: MatrizPar, m2: MatrizPar): MatrizPar = {
    require(m1.headOption.exists(_.length == m2.length), "Dimensiones incompatibles")
    val m2TransPar = transposeParD(m2)
    val n = m1.length
    ParVector.tabulate(n, m2TransPar.length) { (i, j) =>
      prodPuntoParD(m1(i), m2TransPar(j))
    }
  }

  // --- Multiplicación Estándar ---

  // 1.1.1 Secuencial
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.headOption.exists(_.length == m2.length), "Dimensiones incompatibles")
    val m2trans = transpuesta(m2)
    val n = m1.length
    val p = m2trans.length
    Vector.tabulate(n, p)((i, j) => prodPunto(m1(i), m2trans(j)))
  }

  // 1.1.2 Paralela
  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m2.length && m1.headOption.forall(_.length == m1.length), "Requiere matrices cuadradas de igual dimensión") // Asumiendo cuadradas para simplificar
    val m2trans = transpuesta(m2)
    val n = m1.length

    val tasks: Seq[ForkJoinTask[Vector[Int]]] =
      for (i <- 0 until n) yield {
        task {
          Vector.tabulate(n)(j => prodPunto(m1(i), m2trans(j)))
        }
      }
    val resultRows: Seq[Vector[Int]] = tasks.map(_.join())
    resultRows.toVector
  }


  // --- Multiplicación Recursiva ---

  // 1.2.3 Secuencial
  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m1(0).length && m2.length == m2(0).length && m1.length == m2.length, "Matrices deben ser cuadradas y de igual dimensión")
    require(m1.nonEmpty && (m1.length & (m1.length - 1)) == 0, "Dimensión debe ser potencia de 2")

    val n = m1.length
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val l = n / 2
      val a11 = subMatriz(m1, 0, 0, l); val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l); val a22 = subMatriz(m1, l, l, l)
      val b11 = subMatriz(m2, 0, 0, l); val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l); val b22 = subMatriz(m2, l, l, l)

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

  // 1.2.4 Paralela
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m1(0).length && m2.length == m2(0).length && m1.length == m2.length, "Matrices deben ser cuadradas y de igual dimensión")
    require(m1.nonEmpty && (m1.length & (m1.length - 1)) == 0, "Dimensión debe ser potencia de 2")

    val n = m1.length
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val l = n / 2
      val a11 = subMatriz(m1, 0, 0, l); val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l); val a22 = subMatriz(m1, l, l, l)
      val b11 = subMatriz(m2, 0, 0, l); val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l); val b22 = subMatriz(m2, l, l, l)

      val t1 = task { multMatrizRecPar(a11, b11) }
      val t2 = task { multMatrizRecPar(a12, b21) }
      val t3 = task { multMatrizRecPar(a11, b12) }
      val t4 = task { multMatrizRecPar(a12, b22) }
      val t5 = task { multMatrizRecPar(a21, b11) }
      val t6 = task { multMatrizRecPar(a22, b21) }
      val t7 = task { multMatrizRecPar(a21, b12) }
      val t8 = task { multMatrizRecPar(a22, b22) }

      val c11 = sumMatriz(t1.join(), t2.join())
      val c12 = sumMatriz(t3.join(), t4.join())
      val c21 = sumMatriz(t5.join(), t6.join())
      val c22 = sumMatriz(t7.join(), t8.join())

      Vector.tabulate(n, n) { (i, j) =>
        if (i < l && j < l) c11(i)(j)
        else if (i < l && j >= l) c12(i)(j - l)
        else if (i >= l && j < l) c21(i - l)(j)
        else c22(i - l)(j - l)
      }
    }
  }

  // --- Multiplicación Strassen ---

  // 1.3.2 Secuencial
  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m1(0).length && m2.length == m2(0).length && m1.length == m2.length, "Matrices deben ser cuadradas y de igual dimensión")
    require(m1.nonEmpty && (m1.length & (m1.length - 1)) == 0, "Dimensión debe ser potencia de 2")

    val n = m1.length
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val l = n / 2
      val a11 = subMatriz(m1, 0, 0, l); val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l); val a22 = subMatriz(m1, l, l, l)
      val b11 = subMatriz(m2, 0, 0, l); val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l); val b22 = subMatriz(m2, l, l, l)

      val s1 = restaMatriz(b12, b22); val s2 = sumMatriz(a11, a12)
      val s3 = sumMatriz(a21, a22); val s4 = restaMatriz(b21, b11)
      val s5 = sumMatriz(a11, a22); val s6 = sumMatriz(b11, b22)
      val s7 = restaMatriz(a12, a22); val s8 = sumMatriz(b21, b22)
      val s9 = restaMatriz(a11, a21); val s10 = sumMatriz(b11, b12)

      val p1 = multStrassen(a11, s1); val p2 = multStrassen(s2, b22)
      val p3 = multStrassen(s3, b11); val p4 = multStrassen(a22, s4)
      val p5 = multStrassen(s5, s6); val p6 = multStrassen(s7, s8)
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

  // 1.3.3 Paralela
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m1(0).length && m2.length == m2(0).length && m1.length == m2.length, "Matrices deben ser cuadradas y de igual dimensión")
    require(m1.nonEmpty && (m1.length & (m1.length - 1)) == 0, "Dimensión debe ser potencia de 2")

    val n = m1.length
    if (n == 1) {

      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val l = n / 2
      val a11 = subMatriz(m1, 0, 0, l); val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l); val a22 = subMatriz(m1, l, l, l)
      val b11 = subMatriz(m2, 0, 0, l); val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l); val b22 = subMatriz(m2, l, l, l)

      val s1 = restaMatriz(b12, b22); val s2 = sumMatriz(a11, a12)
      val s3 = sumMatriz(a21, a22); val s4 = restaMatriz(b21, b11)
      val s5 = sumMatriz(a11, a22); val s6 = sumMatriz(b11, b22)
      val s7 = restaMatriz(a12, a22); val s8 = sumMatriz(b21, b22)
      val s9 = restaMatriz(a11, a21); val s10 = sumMatriz(b11, b12)

      val p1 = task { multStrassenPar(a11, s1) }
      val p2 = task { multStrassenPar(s2, b22) }
      val p3 = task { multStrassenPar(s3, b11) }
      val p4 = task { multStrassenPar(a22, s4) }
      val p5 = task { multStrassenPar(s5, s6) }
      val p6 = task { multStrassenPar(s7, s8) }
      val p7 = task { multStrassenPar(s9, s10) }

      val c11 = sumMatriz(restaMatriz(sumMatriz(p5.join(), p4.join()), p2.join()), p6.join())
      val c12 = sumMatriz(p1.join(), p2.join())
      val c21 = sumMatriz(p3.join(), p4.join())
      val c22 = restaMatriz(restaMatriz(sumMatriz(p5.join(), p1.join()), p3.join()), p7.join())

      Vector.tabulate(n, n) { (i, j) =>
        if (i < l && j < l) c11(i)(j)
        else if (i < l && j >= l) c12(i)(j - l)
        else if (i >= l && j < l) c21(i - l)(j)
        else c22(i - l)(j - l)
      }
    }
  }

}