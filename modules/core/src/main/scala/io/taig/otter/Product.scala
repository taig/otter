package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Codec.Result
import cats.data.Chain
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import cats.data.Validated
import cats.Invariant

abstract class Product[+O, A] extends Codec[O, A]:
  self =>

  def codecs: Chain[Codec[?, ?]]

  final override def modifyMetadata(f: Metadata => Metadata): Product[O, A] = new Product[O, A]:
    export self.{codecs, decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Product[O, A] = new Product[O, A]:
    export self.{codecs, encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(values: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], A)] =
      (values, default) match
        case (None, Some(default)) => (values, default).valid
        case _                     => self.decode(values)

  final override def imap[B](f: A => B)(g: B => A): Product[O, B] = new Product[O, B]:
    export self.{codecs, metadata}
    override def default: Option[B] = self.default.map(f)
    override def encode(b: B): Format[this.type] = self.encode(g(b))
    override def decode(values: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], B)] =
      self.decode(values).map(_.map(f))

  final def zipWith[P, B](codec: Product[P, B]): Product[O | P, (A, B)] = ???
  // new Product[O | P, (A, B)]:
  //   override def codecs: Chain[Codec[?, ?]] = self.codecs ++ codec.codecs
  //   override def metadata: Metadata = Metadata.Empty
  //   override def default: Option[(A, B)] = None
  //   override def encodeArray(ab: (A, B)): Option[Data.Array[?]] =
  //     (self.encodeArray(ab._1), codec.encodeArray(ab._2)) match
  //       case (Some(left), Some(right)) => Some(left ++ right)
  //       case (Some(left), None)        => Some(left ++ Data.Array.fill(toProduct.codecs.length)(Data.Null))
  //       case (None, Some(right))       => Some(Data.Array.fill(self.codecs.length)(Data.Null))
  //       case (None, None)              => Some(Data.Array.fill(this.codecs.length)(Data.Null))
  //   override def decode(values: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], (A, B))] = values
  //     .toValid(Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String("null"))))
  //     .andThen: values =>
  //       self.decode(values.some) match
  //         case Validated.Valid((values, a)) => codec.decode(values).map(_.tupleLeft(a))
  //         case Validated.Invalid(violations) =>
  //           codec.decode(values.drop(self.codecs.length.toInt).some).fold(violations.combine, _ => violations).invalid

  final def zip[P, B](codec: Product[P, B])(using merge: Evidence.Merge[A, B]): Product[O | P, merge.Out] =
    zipWith(codec).imap(merge.apply)(merge.unapply)

  // final def :*[B](codec: Codec[?, B])(using merge: Evidence.Merge[A, B]): Product[O | codec.type, merge.Out] =
  //   self.zip(codec.toProduct)

  // final def *:[B](codec: Codec[?, B])(using merge: Evidence.Merge[B, A]): Product[codec.type | O, merge.Out] =
  //   codec.toProduct.zip(self)

  final override def optional: Product[O, Option[A]] = new Product[O, Option[A]]:
    export self.{codecs, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def encode(a: Option[A]): Format[this.type] = a.map(self.encode).getOrElse(Data.Null)
    override def decode(values: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], Option[A])] =
      values.fold((values, default.flatten).valid)(_ => self.decode(values).map(_.map(_.some)))

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
        else decode(values.some)
      case Data.Null => decode(none)
      case _         => Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))).invalid
    .map { case (_, a) => a }

  protected def decode(values: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], A)]

object Product:
  val Empty: Product[Nothing, Unit] = new Product[Nothing, Unit]:
    override def codecs: Chain[Codec[?, ?]] = Chain.empty
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Unit] = None
    override def decode(values: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], Unit)] =
      (values, ()).valid
    override def encode(a: Unit): Format[this.type] = Data.Array.Empty

  def apply[A](of: Codec[?, A]): Product[of.type, A] = new Product[of.type, A]:
    override def codecs: Chain[Codec[?, ?]] = Chain.one(of)
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[A] = None
    override def decode(values: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], A)] =
      values
        .toValid(Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String("null"))))
        .andThen(
          _.uncons.toValid(
            Violations.rootNec(Violation(Constraint.Collection.MinItems(reference = 1), actual = Data.Number(0)))
          )
        )
        .andThen { case (head, tail) => of.decode(head).tupleLeft(tail.some) }
    override def encode(a: A): Format[this.type] = Data.Array.one(of.encode(a))

  given [O]: Invariant[Product[O, *]] with
    override def imap[A, B](fa: Product[O, A])(f: A => B)(g: B => A): Product[O, B] = fa.imap(f)(g)
