object Helpers {
  def listOfListsToString(data: List[List[String]]): String = {
    data.foldLeft("")((acc, list) => acc + list.mkString(",") + "\n")
  }
}