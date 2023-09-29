package io.taig.otter.sample.repository

import cats.data.Chain
import cats.effect.IO
import cats.syntax.all.*
import cats.effect.std.AtomicCell
import io.taig.otter.sample.data.Member
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

  val list: IO[Chain[Member.Summary]] = storage.get.map(_.map(_.toSummary))

object MemberRepository:
  object Error:
    enum Create extends NoStackTrace:
      case EmailConflict
