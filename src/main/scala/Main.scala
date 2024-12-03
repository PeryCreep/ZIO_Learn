import zio.{Console, Random, Scope, Task, ZIO, ZIOAppArgs, ZIOAppDefault}

import scala.io.{Source, StdIn}

object Main extends ZIOAppDefault {


  val effect1 = ZIO.attempt(StdIn.readLine("effect1"))
  val effect2 = ZIO.fail(StdIn.readLine("effect2"))

  def getFileSource(filePath: String): Task[Source] = ZIO.attempt {
    Source.fromFile(filePath)
  }

  def getLinesFromSource(source: Source): Task[String] = ZIO.attempt {
    try {
      source.getLines().mkString
    } finally source.close()
  }

  def readFile(file: String): Task[String] = for {
    source <- getFileSource(file)
    lines <- getLinesFromSource(source)
  } yield lines

  def printLine(line: String): Task[Unit] = ZIO.attempt(println(line))

  val readLine: Task[String] = ZIO.attempt(scala.io.StdIn.readLine())

  val random: Task[Int] = ZIO.attempt(scala.util.Random.nextInt(3) + 1)

  def getCacheValue(key: String, onSuccess: String => Unit, onFailure: Throwable => Unit): Unit = ???

  def getCacheValueZio(key: String): ZIO[Any, Throwable, String] =
    ZIO.async { callback =>
      getCacheValue(
        key,
        value => callback(ZIO.succeed(value)),
        error => callback(ZIO.fail(error))
      )
    }

  trait User

  def saveUserRecord(user: User, onSuccess: () => Unit, onFailure: Throwable => Unit): Unit = ???

  def saveUserRecordZio(user: User): ZIO[Any, Throwable, Unit] =
    ZIO.async { callback =>
      saveUserRecord(
        user,
        () => callback(ZIO.unit),
        error => callback(ZIO.fail(error))
      )
    }

  import scala.concurrent.{ExecutionContext, Future}
  trait Query
  trait Result

  def doQuery(query: Query)(implicit ec: ExecutionContext): Future[Result] = ???

  def doQueryZio(query: Query): ZIO[Any, Throwable, Result] =
    ZIO.fromFuture(implicit ec => doQuery(query))

  def helloHuman(): Task[Unit] = {
    for {
      name <- Console.readLine("What is your name?")
      _ <- Console.printLine(s"Hello, $name!")
    } yield ()
  }

  def guessNumberProgram(): Task[Unit] = for {
      int <- Random.nextIntBetween(1, 3)
      _ <- printLine("Guess a number from 1 to 3:")
      num <- Console.readLine
      _ <- if(num == int.toString) printLine("You guessed right!") else printLine(s"You guessed wrong, the number was $int!")
    } yield ()

  import java.io.IOException
  def readUntil(acceptInput: String => Boolean): ZIO[Any, IOException, String] =
    for {
      readLine <- Console.readLine("Write your line")
      result <- if(acceptInput(readLine)) ZIO.succeed(readLine) else readUntil(acceptInput)
    } yield result

  def doWhile[R, E, A](body: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] =
    for {
      bodyResult <- body
      result <- if(condition(bodyResult)) doWhile(body)(condition) else ZIO.succeed(bodyResult)
    } yield result

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    doWhile(Console.printLine("I executed, because condition is true"))(_ => true)
  }
  
  
}
