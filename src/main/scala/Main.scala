import java.io.PrintWriter

object Main extends App {
  val fileNames = List("a_example", "b_read_on", "c_incunabula", "d_tough_choices", "e_so_many_books", "f_libraries_of_the_world")
  val file = fileNames(0)

  val printWriter = new PrintWriter(s"output/$file.txt")
  val lines = Calculator.readFromFile(file).split("\n").toList
  val (daysLeft, books, libraries) = Calculator.extractData(lines)

  val scanner = Scanner(libraries, books, daysLeft)

  for (i <- 1 to daysLeft) {
    scanner.next(i)
  }
  scanner.sendBooks()

  printWriter.close()
}
