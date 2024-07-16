package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Codec.Result

abstract class Dynamic[A] extends Codec[Nothing, A]:
  self =>

  override def imap[B](f: A => B)(g: B => A): Dynamic[B] = new Dynamic[B]:
    export self.metadata
    override def decodeOption(data: Option[Data.Value]): Codec.Result[Data, B] =
      self.decodeOption(data).map(f)
    override def encodeOption(b: B): Option[Data.Value] = self.encodeOption(g(b))

  override def optional: Dynamic[Option[A]] = new Dynamic[Option[A]]:
    export self.metadata
    override def decodeOption(data: Option[Data.Value]): Codec.Result[Data, Option[A]] =
      data.fold(none.valid)(_ => self.decodeOption(data).map(_.some))
    override def encodeOption(a: Option[A]): Option[Data.Value] = a.flatMap(self.encodeOption)

  override def update(f: Metadata => Metadata): Dynamic[A] = new Dynamic[A]:
    export self.{decodeOption, encodeOption}
    override def metadata: Metadata = f(metadata)

object Dynamic:
  val Default: Dynamic[Data] = new Dynamic[Data]:
    override def metadata: Metadata = Metadata.Empty
    override def decodeOption(data: Option[Data.Value]): Codec.Result[Data, Data] =
      data.getOrElse(Data.Null).valid
    override def encodeOption(a: Data): Option[Data.Value] = a.toValue
