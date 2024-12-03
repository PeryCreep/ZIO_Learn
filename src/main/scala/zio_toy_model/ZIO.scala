package zio_toy_model

final case class ZIO[-R, +E, +A](run: R => Either[E, A])

object ZIO {

  def succeed[A](value: => A): ZIO[Any, Nothing, A] = ZIO(_ => Right(value))
  def fail[E](error: => E): ZIO[Any, E, Nothing] = ZIO(_=> Left(error))

  def zipWith[R, E, A, B, C](self: ZIO[R, E, A], that: ZIO[R, E, B])(f: (A, B) => C): ZIO[R, E, C] =
    ZIO(r =>
      for {
        selfResult <- self.run(r)
        thatResult <- that.run(r)
      } yield f(selfResult, thatResult)
    )
  //    ZIO(r => self.run(r).flatMap(selfResult => that.run(r).map(thatRes => f(selfResult, thatRes))))

  def collectAll[R, E, A](in: Iterable[ZIO[R, E, A]]): ZIO[R, E, List[A]] =
    ZIO(r => in.map(_.run(r)).foldLeft[Either[E, List[A]]](Right[E, List[A]](List.empty[A])) {
      case (Right(list), Right(value)) =>
        Right(list :+ value)
      case (_, Left(error)) =>
        Left(error)
    })

  def foreach[R, E, A, B](in: Iterable[A])(f: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
    collectAll(in.map(f))

  def orElse[R, E1, E2, A](self: ZIO[R, E1, A], that: ZIO[R, E2, A]): ZIO[R, E2, A] =
    ZIO(r => self.run(r) match {
      case Left(value) => that.run(r)
      case Right(value) => Right(value)
    })

  //  def eitherToZIO[E, A](either: Either[E, A]): ZIO[Any, E, A] = {
  //    either match
  //      case Left(error) => ZIO.fail(error)
  //      case Right(value) => ZIO.succeed(value)
  //  }

  def listToZIO[A](list: List[A]): ZIO[Any, None.type, A] = list.headOption match {
    case Some(value) => ZIO.succeed(value)
    case None => ZIO.fail(None)
  }
}