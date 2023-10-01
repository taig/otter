package io.taig.otter.sample.repository

import cats.data.Chain
import cats.effect.IO
import cats.effect.std.AtomicCell
import cats.syntax.all.*
import io.taig.otter.sample.data.{Librarian, Member, ReferenceOrSelf}
import io.taig.otter.sample.repository.MemberRepository.Error
import io.taig.otter.sample.service.ReferenceGenerator

import scala.util.control.NoStackTrace

final class MemberRepository(references: ReferenceGenerator, storage: AtomicCell[IO, Chain[Member]]):
  def create(member: Member.Create): IO[Either[Error.Create, Member.Summary]] = storage
    .evalModify { members =>
      val verifyEmail: IO[Unit] =
        IO.raiseWhen(members.exists(_.email === member.email))(Error.Create.EmailConflict)

      val generateReference: IO[Member.Reference] =
        references.generate(Member.Reference.Length).map(Member.Reference.unsafeFromCIString)

      for
        _ <- verifyEmail
        reference <- generateReference
        value = Member(reference, member.email, member.password, session = none)
      yield (members :+ value, value.toSummary)
    }
    .attemptNarrow[Error.Create]

  def findByReference(
      reference: ReferenceOrSelf[Member.Reference],
      self: Member | Librarian.Summary
  ): IO[Either[Error.FindByReference, Member.Summary]] = (reference, self)
    .match {
      case (ReferenceOrSelf.Self, self: Member)         => IO.pure(self.toSummary)
      case (ReferenceOrSelf.Self, _: Librarian.Summary) => IO.raiseError(Error.FindByReference.MemberReferenceUnknown)
      case (ReferenceOrSelf.Reference(reference), self: Member) =>
        if reference === self.reference
        then IO.pure(self.toSummary)
        else IO.raiseError(Error.FindByReference.PermissionDenied)
      case (ReferenceOrSelf.Reference(reference), self: Librarian.Summary) =>
        storage.get
          .map(_.find(_.reference === reference))
          .flatMap(_.liftTo[IO](Error.FindByReference.MemberReferenceUnknown))
          .map(_.toSummary)
    }
    .attemptNarrow[Error.FindByReference]

  val list: IO[Chain[Member.Summary]] = storage.get.map(_.map(_.toSummary))

object MemberRepository:
  object Error:
    enum Create extends NoStackTrace:
      case EmailConflict

    enum FindByReference extends NoStackTrace:
      case PermissionDenied
      case MemberReferenceUnknown
