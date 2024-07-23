package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.validation.Validation
import io.taig.otter.Codec.Result

sealed abstract class Primitive[A] extends Value[Nothing, A]:
  self =>

  override def modifyMetadata(f: Metadata => Metadata): Primitive[A]

  override def modifyDefault(f: Option[A] => Option[A]): Primitive[A]

  override def imap[B](f: A => B)(g: B => A): Primitive[B] = ivalidate(Validation.lift(f))(g)

  def ivalidate[B](validation: CodecValidation.Primitive[A, B])(f: B => A): Primitive[B]

  final override def optional: Primitive[Option[A]] = ???

object Primitive:
  sealed abstract class Optional[A] extends Primitive[A] {
    self =>

    override def modifyMetadata(f: Metadata => Metadata): Primitive[A] = new Optional[A]:
      export self.encode
      override def default: Option[A] = ???
      override def decode(data: Data): Result[A] = ???
      override def metadata: Metadata = ???

    override def modifyDefault(f: Option[A] => Option[A]): Primitive[A] = ???

    override def ivalidate[B](validation: CodecValidation.Primitive[A, B])(f: B => A): Primitive[B] = ???
  }

  sealed abstract class Required[A] extends Primitive[A], Value.Required[Nothing, A]:
    self =>

    final override def modifyMetadata(f: Metadata => Metadata): Primitive.Required[A] = new Required[A]:
      export self.{decodeValue, default, encode}
      override def metadata: Metadata = f(self.metadata)

    final override def modifyDefault(f: Option[A] => Option[A]): Primitive.Required[A] = new Required[A]:
      export self.{encode, metadata}
      override def default: Option[A] = f(self.default)

      override def decodeValue(data: Data.Value): Result[A] = ???

    final override def imap[B](f: A => B)(g: B => A): Primitive.Required[B] = ivalidate(Validation.lift(f))(g)

    override def ivalidate[B](validation: CodecValidation.Primitive[A, B])(f: B => A): Primitive.Required[B] =
      new Required[B]:
        export self.metadata
        override def default: Option[B] = self.default.flatMap(validation(_).toOption)
        override def decodeValue(data: Data.Value): Codec.Result[B] =
          self.decodeValue(data).andThen(validation(_).leftMap(Violations.root))
        override def encode(b: B): Format[this.type] = self.encode(f(b))

  object Required:
    given ValidationInvariant[Constraint.Primitive, Primitive.Required] with

      extension [A](self: Primitive.Required[A])
        override def ivalidate[B](validation: CodecValidation.Primitive[A, B])(f: B => A): Primitive.Required[B] =
          self.ivalidate(validation)(f)

  def apply[A](tpe: Type[A]): Primitive.Required[A] = new Required[A]:
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[A] = None
    override def decodeValue(data: Data.Value): Codec.Result[A] = data.toPrimitive
      .flatMap(tpe.decode)
      .toValid(Violations.rootNec(Violation(Constraint.Type(tpe.name), actual = Data.String(data.name))))
    override def encode(a: A): Format[this.type] = tpe.encode(a)

  given ValidationInvariant[Constraint.Primitive, Primitive] with
    extension [A](self: Primitive[A])
      override def ivalidate[B](validation: CodecValidation.Primitive[A, B])(f: B => A): Primitive[B] =
        self.ivalidate(validation)(f)
