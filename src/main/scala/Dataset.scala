import scala.io.Source
import Helpers.listOfListsToString

class Dataset(m: List[List[String]]) {
  var data: List[List[String]] = m
  override def toString: String = listOfListsToString(data)

  def selectColumn(col: String): Dataset = {
    val index = getHeader.indexOf(col)
    if (index != -1){
      val selectedColumn = data.map(row => List(row(index)))
      new Dataset(selectedColumn)
    }
    else {
      throw new IllegalArgumentException(s"Column $col does not exist")
    }
  }

  def selectColumns(cols: List[String]): Dataset = {
    val datasets = cols.map(selectColumn)

    val data = datasets.map(_.data.flatten).transpose

    val dataset = new Dataset(data)
    dataset
  }

  def getFirstColumn: List[String] = {
    val selectedColumn = data.map(row => row.head)
    selectedColumn
  }

  private def addRow(row: List[String]): Unit = {
    data = data :+ row
  }

  def split(percentage: Double): (Dataset, Dataset) = {
    val sortedData = data.tail.sortBy((row: List[String]) => row.head)
    val raport = (1 / percentage).ceil - 1
    var counter = 0;
    val testData = new Dataset(Nil);
    val trainingData = new Dataset(Nil)
    testData.addRow(data.head)
    trainingData.addRow(data.head)
    sortedData.foreach { row =>
      if (counter < raport) {
        trainingData.addRow(row)
        counter = counter + 1
      } else {
        testData.addRow(row)
        counter = 0
      }
    }
    (trainingData, testData)
  }

  def size: Int = data.size
  def getRows: List[List[String]] = {
    data.tail
  }
  def getHeader: List[String] = {
    data.head
  }
}

object Dataset {
  def apply(csv_filename: String): Dataset = {
    val source = Source.fromFile(csv_filename)
    val dataset = new Dataset(source.getLines().map(_.split(",").toList).toList)
    source.close()
    dataset
  }
  def apply(ds: List[List[String]]): Dataset = {
    new Dataset(ds)
    }
}