import Main.printWriter
import scala.collection.mutable.{ListBuffer, Map}

case class Scanner(var libraryList: List[Library], var books: Vector[Int], daysLeft: Int){
  private var currentLibrary: Option[Library] = None
  private val readyList: Map[Int, Library] = Map()
  private val allBooks = books
  private var lockCounter: Int = 0

  def next(day: Int): Unit = {
    checkCurrentLibraryAndMoveToReadyList(day)

    if (lockCounter == 0 && libraryList.nonEmpty) {
      currentLibrary match {
        case Some(value) => readyList.put(day, value)
        case None => None
      }

      currentLibrary = Some(libraryList.head)

      val signupTime = currentLibrary.head.signupTime
      lockCounter = signupTime

      setUsedBooksToZero()

      libraryList = Calculator.calculateLibraries(libraryList.tail, books, day)
    }

    lockCounter -= 1
  }

  private def checkCurrentLibraryAndMoveToReadyList(day: Int): Unit = {
    if (lockCounter == 1 && libraryList.isEmpty) {
      currentLibrary match {
        case Some(value) =>
          readyList.put(day, value)
          currentLibrary = None
        case None => None
      }
    }
  }

  def sendBooks(): Unit = {
    //print number of libraries that will send books
    printWriter.println(readyList.size)
    readyList.toSeq.sortBy(_._1).foreach {
      case (readyDay, library) =>
        val sentBooks = getBooksToSend(library, readyDay)
        //print library number and book count
        printWriter.println(s"${library.index} ${sentBooks.size}")
        //print book sequence
        printWriter.println(s"${sentBooks.mkString(" ")}")
    }
  }

  private def getBooksToSend(library: Library, readyDay: Int): List[Int] = {
    val remainingDays = daysLeft - readyDay + 1
    val bookCapacity: Long = library.capacity.toLong * remainingDays.toLong
    //sort by value, take the ones you can send
    val booksValue = library.books.map(index => {
      (index, allBooks(index))
    })
      .sortWith(_._2 > _._2)
      .map(_._1)

    if (booksValue.size < bookCapacity) {
      booksValue
    } else {
      booksValue.take(bookCapacity.toInt)
    }
  }

  private def setUsedBooksToZero(): Unit = {
    val list: ListBuffer[Int] = new ListBuffer[Int]()
    for (i <- books.indices) {
      list += (if (currentLibrary.head.books.contains(i)) 0 else books(i))
    }
    books = list.toVector
  }
}
