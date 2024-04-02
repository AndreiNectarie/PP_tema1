object Regression {

  def regression(dataset_file: String,
                attribute_columns: List[String],
                value_column: String,
                test_percentage: Double,
                alpha: Double,
                gradient_descent_steps: Int): (Matrix, Double) = {
    val dataset: Dataset = Dataset(dataset_file)
    val (trainingData, testData) = dataset.split(test_percentage)
    val trainingMatrix = Matrix(Dataset(trainingData.data.tail))
    trainingMatrix++(1)
    trainingMatrix.height match {
      case None => None
      case Some(x) => val size = x
    }
    //var parameterArray: List[List[Double]] = List[List.fill(size)(0)]
    (trainingMatrix, 2)
  }

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}