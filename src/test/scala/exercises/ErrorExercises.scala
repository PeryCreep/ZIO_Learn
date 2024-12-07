package exercises

import zio._
import zio.test._
import zio.test.Assertion._

object ErrorExercises extends ZIOSpecDefault {

  def recoverFromSomeDefects[R, E, A](zio: => ZIO[R, E, A])(f: Throwable => Option[A]): ZIO[R, E, A] = {
    zio.foldCauseZIO (
      cause => {
         cause.defects.flatMap(f).map(ZIO.succeed(_)).headOption.getOrElse(ZIO.failCause(cause))
      },
      ZIO.succeed(_)
    )
  }

  def logFailures[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = zio.foldCauseZIO(
    cause => {
      Console.printLine(cause.prettyPrint).orDie *> zio
    },
    _ => zio
  )

  lazy val failedZIO = ZIO.fail("fail")
  lazy val succeedZIO = ZIO.succeed("succeed")
  lazy val dieZIO = ZIO.fail(throw new NumberFormatException())

  override def spec: Spec[Any, Any] = suite("Exercises in Error Model Chapter") (
    test("ZIO effect must fails not dies") {
      assertZIO(ZIO.attempt(throw new Error("test 1")).exit)(fails(anything))
    },
    test("recoverFromSomeDefects must recover any function") {
      val zioEffectFromError = recoverFromSomeDefects(ZIO.fail(throw new Error())){_ => Some("succeed")}
      val zioEffectFromNumberFormatException = recoverFromSomeDefects(ZIO.fail(throw new NumberFormatException())){_ => Some("succeed")}
      assertZIO(zioEffectFromError)(equalTo("succeed"))
      assertZIO(zioEffectFromNumberFormatException)(equalTo("succeed"))
    },
    test("recoverFromSomeDefects must recover") {
      assertZIO(recoverFromSomeDefects(ZIO.fail(throw new Error())){_ => Some("succeed")}.exit)(succeeds(anything))
    },
    test("logFailures must log to console") {
      for {
        result <- logFailures(failedZIO).exit
        line <- TestConsole.output
      } yield assert(line.head)(
        startsWithString(
          result.causeOption
            .flatMap(_.prettyPrint.split('\n').headOption)
            .get
        )
      )
    },
    test("logFailures shouldn't log to console") {
      for {
        _ <- logFailures(succeedZIO).exit
        line <- TestConsole.output
      } yield assert(line)(isEmpty)
    }
  )
}
