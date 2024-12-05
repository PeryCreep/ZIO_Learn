package errors

import zio.ZIO

class MapError {
  final case class InsufficientPermission(user: String, operation: String)

  final case class FileIsLocked(file: String)

  def shareDocument(doc: String): ZIO[Any, InsufficientPermission, Unit] = ???

  def moveDocument(doc: String, folder: String): ZIO[Any, FileIsLocked, Unit] = ???

  type DocumentError = Either[InsufficientPermission, FileIsLocked]//в scala 3 можно использовать union type

  lazy val result: ZIO[Any, DocumentError, Unit] =
    shareDocument("347823")
      .mapError(Left(_))
      .zip(moveDocument("347823", "/temp/").mapError(Right(_)))
      .unit

}
