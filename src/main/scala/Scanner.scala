import Main.printWriter
import scala.collection.mutable.ListBuffer

case class Scanner(var libraryList: List[Library], var books: Vector[Int], daysLeft: Int){
  private var linesToSend: List[String] = List()
  private var lockCounter: Int = 0
  private var totalScore: Int = 0

  def next(day: Int): Unit = {
    if (lockCounter == 0 && libraryList.nonEmpty) {
      val currentLibrary = libraryList.head
      lockCounter = currentLibrary.signupTime

      prepareOutput(currentLibrary, day)
      setUsedBooksToZero(currentLibrary)

      libraryList = Calculator.calculateLibraries(libraryList.tail, books, daysLeft - lockCounter)
    }

    lockCounter -= 1
  }

  private def prepareOutput(library: Library, day: Int): Unit = {
    val sentBooks = getBooksToSend(library, day)
    linesToSend = linesToSend :+ s"${library.index} ${sentBooks.size}" :+ s"${sentBooks.mkString(" ")}"
  }

  def sendBooks(): Unit = {
    //print number of libraries that will send books
    printWriter.println(linesToSend.grouped(2).size)
    linesToSend.foreach(printWriter.println)
    println(totalScore)
  }

  private def getBooksToSend(library: Library, readyDay: Int): List[Int] = {
    val remainingDays = daysLeft - readyDay + 1
    val bookCapacity: Long = library.capacity.toLong * remainingDays.toLong
    //sort by value, take the ones you can send
    val booksValue = library.books.map(index => {
      (index, books(index))
    })
      .sortWith(_._2 > _._2)

    if (booksValue.size < bookCapacity) {
      totalScore += booksValue.map(_._2).sum
      booksValue.map(_._1)
    } else {
      totalScore += booksValue.take(bookCapacity.toInt).map(_._2).sum
      booksValue.take(bookCapacity.toInt).map(_._1)
    }
  }

  private def setUsedBooksToZero(library: Library): Unit = {
    val list: ListBuffer[Int] = new ListBuffer[Int]()
    for (i <- books.indices) {
      list += (if (library.books.contains(i)) 0 else books(i))
    }
    books = list.toVector
  }
}
