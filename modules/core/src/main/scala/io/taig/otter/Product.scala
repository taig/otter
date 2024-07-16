package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Codec.Result
import cats.data.Chain
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

abstract class Product[+O, A] extends Codec[O, A]:
  self =>

  def codecs: Chain[Codec[?, ?]]

  final override def metadata(f: Metadata => Metadata): Product[O, A] = new Product[O, A]:
    export self.{codecs, decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def default(f: Option[A] => Option[A]): Product[O, A] = new Product[O, A]:
    export self.{codecs, encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(values: Option[Chain[Data]]): Codec.Result[(Option[Chain[Data]], A)] =
      (values, default) match
        case (None, Some(default)) => (values, default).valid
        case _                     => self.decode(values)

  final override def imap[B](f: A => B)(g: B => A): Product[O, B] = ???

  final def zip[P, B](product: Product[P, B]): Product[O & P, (A, B)] = ???

  final override def optional: Product[O, Option[A]] = new Product[O, Option[A]]:
    export self.{codecs, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)
    override def decode(values: Option[Chain[Data]]): Codec.Result[(Option[Chain[Data]], Option[A])] =
      values.fold((values, none).valid)(_ => self.decode(values).map(_.map(_.some)))

  final override def decode(data: Data): Codec.Result[A] = data
    .match
      case Data.Array(values) =>
        val actual = values.length
        val expected = codecs.length.toInt

        if actual < expected
        then
          Violations.rootNec(Violation(Constraint.Collection.MinItems(expected), actual = Data.Number(actual))).invalid
        else if actual > expected
        then
          Violations.rootNec(Violation(Constraint.Collection.MaxItems(expected), actual = Data.Number(actual))).invalid
        else decode(Chain.fromSeq(values).some)
      case Data.Null => decode(none[Chain[Data]])
      case _         => Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))).invalid
    .map { case (_, a) => a }

  protected def decode(values: Option[Chain[Data]]): Codec.Result[(Option[Chain[Data]], A)]

object Product:
  val Empty: Product[Nothing, Unit] = new Product[Nothing, Unit]:
    override def codecs: Chain[Codec[?, ?]] = Chain.empty
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Unit] = None
    override def decode(values: Option[Chain[Data]]): Codec.Result[(Option[Chain[Data]], Unit)] =
      (values, ()).valid
    override def encode(a: Unit): Data = Data.Array.Empty
