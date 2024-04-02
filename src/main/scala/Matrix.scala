type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {
  def transpose: Matrix = {
    data match {
      case None => new Matrix(None)
      case Some(x) => new Matrix(Some(x.transpose))
    }
  }
  def map(f: Double => Double): Matrix = {
    data match {
      case None => new Matrix(None)
      case Some(x) => new Matrix(Some(x.map(row => row.map(f))))
    }
  }

  private def op(a: Mat, b:Mat): Matrix = {
    val result = a.map(row =>
      b.transpose.map(col =>
        row.zip(col).map((x, y) => x * y).sum
      )
    )
    new Matrix(Some(result))
  }
  def *(other: Matrix): Matrix = {
    data match {
      case None => new Matrix(None)
      case Some(x) =>
        other.data match {
          case None => new Matrix(None)
          case Some(y) =>
            if (x.head.length == y.length) {
              op(x, y)
            }
            else {
              new Matrix(None)
            }

        }
    }
  }

  def ++(x: Double): Matrix = {
    data match {
      case None => new Matrix(None)
      case Some(y) => new Matrix(Some(y.map(row => row :+ x)))
    }
  }
  def -(other: Matrix): Matrix = {
    data match {
      case None => new Matrix(None)
      case Some(x) =>
        other.data match {
          case None => new Matrix(None)
          case Some(y) =>
            if (x.length == y.length && x.head.length == y.head.length) {
              new Matrix(Some(subtractMatrices(x, y)))
            }
            else {
              new Matrix(None)
            }
        }
    }
  }

  
  
    private def subtractMatrices(a: Mat, b: Mat): Mat = {
      val result = a.zip(b).map { (row_a, row_b) =>
        row_a.zip(row_b).map((elem_a, elem_b) => elem_a - elem_b)
      }
      result
    }

  def data: Option[Mat] = m
  def height: Option[Int] = {
    data match {
      case Some(x) => Some(x.length)
      case None => None
    }
  }
  def width: Option[Int] = {
    data match {
      case Some(x) => Some(x.head.length)
      case None => None
    }
  }

  override def toString: String = {
    data.map(row => row.mkString("[", ", ", "]")).mkString("\n")
  }
}

object Matrix {
  def apply(data: Mat): Matrix = {
    new Matrix(Some(data))
  }
  def apply(data: Option[Mat]): Matrix = {
    new Matrix(data)
  }
  def apply(dataset: Dataset): Matrix = {
    val data = dataset.data
    val m = data.map(_.map(_.toDouble))
    new Matrix(Some(m))
  }
}
