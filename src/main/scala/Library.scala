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
      calculateScore(signupTime, capacity, daysLeft, books, allBooks)
    )
  }

  private def calculateScore(signupTime: Int, capacity: Int, daysLeft: Int, books: List[Int], allBooks: Vector[Int]) = {
    // This is far too simple, needs to be a little bit more sophisticated
    // good things: total books value, library daily capacity
    // bad things: signup time, days left for submission

    // input c scores better with
    //val num = math.sqrt(books.map(allBooks(_)).sum * capacity)
    //val den = math.pow(signupTime.toDouble, 9.0) / daysLeft.toDouble

    // input e scores better with
    //val num = math.sqrt(books.map(allBooks(_)).sortWith(_ > _).take(daysLeft * capacity).sum * capacity)
    //val den = signupTime.toDouble / daysLeft.toDouble

    //everything else
    val num = books.map(allBooks(_)).sortWith(_ > _).take(daysLeft * capacity).sum
    val den = signupTime.toDouble / daysLeft.toDouble

    num / den
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
