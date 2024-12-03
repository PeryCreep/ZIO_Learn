package errors


import zio._
import zio.test.Assertion._
import zio.test._

object ErrorsFormatTests extends ZIOSpecDefault {


  override def spec: Spec[Any, Nothing] = suite("ExampleSpec") (
    test("ZIO.succeed dies with ArithmeticException") {
      assertZIO(ZIO.succeed(1 / 0).exit)(dies(isSubtype[ArithmeticException](anything)))
    },
    test("ZIO.succeed dies with NumberFormatException") {//логично, так как любая ошибка, которая была выброшена,
      // является ошибкой, которую мы не ожидаем по типу ,
      // но если мы просто вернем new Throwable, то это значит, что ошибка пойдет в сигнатуру эффекта и мы можем обработать данный кейс,
      // как в тесте ниже
      assertZIO(ZIO.fail(throw new NumberFormatException()).exit)(dies(isSubtype[NumberFormatException](anything)))
    },
    test("ZIO.fail must fails with NumberFormatException") {
      assertZIO(ZIO.fail(new NumberFormatException()).exit)(fails(isSubtype[NumberFormatException](anything)))
    },
    test("ZIO.succeed must not be succeed") {
      assertZIO(ZIO.fail(throw new NumberFormatException()).exit)(!succeeds(anything))
    },
    test("ZIO.succeed must be succeed") {
      assertZIO(ZIO.succeed("Test").exit)(succeeds(anything))
    },
    test("ZIO.fail with fold must be succeed") {
      assertZIO(ZIO.fail(new RuntimeException()).foldCauseZIO(_ => ZIO.succeed("cause"), _ => ZIO.succeed("succeed")).exit
      )(succeeds(equalTo("cause")))
    },
    test("ZIO.fail with orDie must die") {
      assertZIO(ZIO.fail(new RuntimeException()).orDie.exit)(dies(isSubtype[RuntimeException](anything)))
    },
    test("ZIO.fail with refineWith exactly error must fails") {
      assertZIO(ZIO.attempt(throw new RuntimeException()).refineOrDie {
        case e: RuntimeException => e
      }.exit)(fails(isSubtype[RuntimeException](anything)))
    },
    test("ZIO.fail with refineWith must die") {
      assertZIO(ZIO.attempt(throw new NumberFormatException()).refineOrDie {
        case e: ArithmeticException => e
      }.exit)(dies(isSubtype[NumberFormatException](anything)))
    }
  )
}
