package io.taig.otter

import cats.data.NonEmptyChain
import cats.data.NonEmptyChainImpl.Type
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.syntax.all.*

sealed abstract class Union[+O <: Data, A] extends Codec[O, A]:
  self =>

  def branches: NonEmptyChain[Branch[?, ?]]

  final override def modifyMetadata(f: Metadata => Metadata): Union[O, A] = new Union[O, A]:
    export self.{branches, decodeBranches, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def imap[B](f: A => B)(g: B => A): Union[O, B] = new Union[O, B]:
    export self.{branches, metadata}
    override def decodeBranches(data: Data): Either[Option[Violations], B] = self.decodeBranches(data).map(f)
    override def encode(b: B): O = self.encode(g(b))

  final override def to[B](using convert: Convert[A, B]): Union[O, B] = imap(convert.to)(convert.from)

  final def orElse[P <: Data, B](codec: => Union[P, B]): Union[O | P, Either[A, B]] = new Union[O | P, Either[A, B]]:
    override def branches: NonEmptyChain[Branch[?, ?]] = self.branches ++ codec.branches
    override def metadata: Metadata = Metadata.Empty
    override def decodeBranches(data: Data): Either[Option[Violations], Either[A, B]] = self.decodeBranches(data) match
      case Right(a) => a.asLeft.asRight
      case Left(left) =>
        codec.decodeBranches(data) match
          case Right(b) => b.asRight.asRight
          case Left(right) =>
            (left, right) match
              case (Some(left), Some(right)) => (left |+| right).some.asLeft
              case (Some(left), None)        => left.some.asLeft
              case (None, Some(right))       => right.some.asLeft
              case (None, None)              => none.asLeft
    override def encode(ab: Either[A, B]): O | P = ab.fold(self.encode, codec.encode)

  // TODO better error message, especially when we have discriminator information
  final override def decode(data: Data): Codec.Result[A] = decodeBranches(data) match
    case Right(a)               => a.valid
    case Left(Some(violations)) => violations.invalid
    case Left(None)             => Violations.rootNec(Violation.tpe(name = "union", actual = data.name)).invalid

  protected def decodeBranches(data: Data): Either[Option[Violations], A]

  final def :+[P <: Data, B](branch: Branch[P, B]): Union[O | P, Either[A, B]] = orElse(branch.toUnion)

  final def +:[P <: Data, B](branch: Branch[P, B]): Union[P | O, Either[B, A]] = branch.toUnion.orElse(self)

object Union:
  extension [O <: Data, A <: Matchable](self: Union[O, A])
    inline def or[P <: Data, B <: Matchable](codec: Union[P, B]): Union[O | P, A | B] = self
      .orElse(codec)
      .imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => a.asLeft
        case b: B => b.asRight
      }

    inline def |[P <: Data, B <: Matchable](branch: Branch[P, B]): Union[O | P, A | B] = or(branch.toUnion)

  def apply[O <: Data, A](branch: Branch[O, A]): Union[O, A] = new Union[O, A]:
    override def branches: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
    override def metadata: Metadata = Metadata.Empty
    override def decodeBranches(data: Data): Either[Option[Violations], A] =
      branch.decode(data).leftMap(branch.name /: _) match
        case Valid(Some(a))      => a.asRight
        case Valid(None)         => none.asLeft
        case Invalid(violations) => violations.some.asLeft

    override def encode(a: A): O = branch.encode(a)
