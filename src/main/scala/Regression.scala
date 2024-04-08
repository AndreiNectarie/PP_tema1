import scala.annotation.tailrec

object Regression {

  private def initialize_parameters(n: Int): Matrix = {
    if (n > 0) {
      new Matrix(Some(List.fill(n)(List.fill(1)(0.0))))
    } else {
      new Matrix(Some(List(List(0.0))))
    }
  }
  @tailrec
  private def gradientDescent(W: Matrix, steps: Int, alpha: Double, trainingMatrix: Matrix, price_column: Matrix): Matrix = {
    if (steps == 0) {
      W
    } else {
      val estimations = trainingMatrix * W
      val error = estimations - price_column
      val gradient = (trainingMatrix.transpose * error) * (1.0/trainingMatrix.height.get)
      val newW = W - (gradient * alpha)
      gradientDescent(newW, steps - 1, alpha, trainingMatrix, price_column)
    }
  }

  def regression(dataset_file: String,
                 attribute_columns: List[String],
                 value_column: String,
                 test_percentage: Double,
                 alpha: Double,
                 gradient_descent_steps: Int): (Matrix, Double) = {
    val dataset: Dataset = Dataset(dataset_file)
    val datasetUsable: Dataset = dataset.selectColumns(attribute_columns :+ value_column)
    val (trainingData, testData) = datasetUsable.split(test_percentage)
    val price_column = trainingData.selectColumn(value_column)
    val price_final_column = Matrix(Dataset(price_column.data))
    val trainingMatrix = Matrix(Dataset(trainingData.selectColumns(attribute_columns).data)) ++ 1
    val W = initialize_parameters(trainingMatrix.width.get)
    val finalW = gradientDescent(W, gradient_descent_steps, alpha, trainingMatrix, price_final_column)
    (finalW, 0.0) // return finalW and a placeholder for the error value
  }

  def main(args: Array[String]): Unit = {
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1,1e-7,10000))
  }
}