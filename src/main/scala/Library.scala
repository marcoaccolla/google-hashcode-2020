case class Library(
  index: Int,
  signupTime: Int,
  capacity: Int,
  books: List[Int],
  score: Double
)

case object Library{
  //from parsed data
  def apply(index: Int, signupTime: Int, capacity: Int, books: List[Int], allBooks: Vector[Int], daysLeft: Int): Library = {
    new Library(
      index,
      signupTime,
      capacity,
      books,
      books.map(index =>
        // library books score * daily capacity / signup time * (signup time / daysLeft)
        allBooks(index)).sum * capacity /
        (signupTime * (signupTime / daysLeft.toDouble))
    )
  }

  //from Library
  def apply(index: Int, library: Library, allBooks: Vector[Int], daysLeft: Int): Library = {
    Library(index, library.signupTime, library.capacity, library.books, allBooks, daysLeft)
  }

  //from raw data
  def apply(index: Int, data: List[String], allBooks: Vector[Int], daysLeft: Int): Library = {
    val libraryData = data.head.split(" ")
    val libraryBooks = data.last.split(" ").map(_.toInt).toList
    val signupTime = libraryData.tail.head.toInt
    val capacity = libraryData.last.toInt

    Library(index, signupTime, capacity, libraryBooks, allBooks, daysLeft)
  }
}
