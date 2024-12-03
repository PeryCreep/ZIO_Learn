import scala.io.StdIn

object ImperativeMain extends App {


  def readLine = {
    StdIn.readLine("effect")
  }

  def effect1 = () => readLine
  def effect2 = readLine

  def printLine(string: String) = {
    println(string)
  }

  (1 to 100).foreach(n => printLine(n.toString))
}
