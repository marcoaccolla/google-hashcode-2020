import scala.io.Source.fromFile

case object Calculator{
  def readFromFile(fileName: String): String = {
    val source = fromFile(s"input/$fileName.txt")
    try source.mkString finally source.close()
  }

  def extractData(lines: List[String]): (Int, Vector[Int], List[Library]) = {
    val daysLeft = lines
      .head
      .split(" ")
      .toList
      .last
      .toInt

    val books = lines
      .tail
      .head
      .split(" ")
      .map(_.toInt)
      .toVector

    val groupedLibraries = lines
      .tail
      .tail
      .grouped(2)
      .toList

    val libraries: List[Library] =
      groupedLibraries
      .map(list => Library(groupedLibraries.indexOf(list), list, books, daysLeft))
      .sortWith(_.score > _.score)

    (daysLeft, books, libraries)
  }

  def calculateLibraries(libraries: List[Library], books: Vector[Int], daysLeft: Int): List[Library] = {
    libraries
      .map(library => Library(library.index, library, books, daysLeft))
      .sortWith(_.score > _.score)
  }
}
