package io.taig.otter.sample.data

import cats.Eq
import cats.syntax.all.*
import io.taig.otter.validation.{validations, Validation}
import org.typelevel.ci.CIString

import java.util.UUID
import java.util.regex.Pattern

final case class Member(
    reference: Member.Reference,
    email: Member.Email,
    password: Member.Password,
    session: Option[Member.Session]
):
  def toSummary: Member.Summary = Member.Summary(reference, email)

object Member:
  opaque type Reference = CIString
  object Reference:
    val Length = 8
    extension (self: Member.Reference) def toCIString: CIString = self
    def unsafeFromCIString(value: CIString): Member.Reference = value
    val validation: Validation[CIString, Member.Reference] = validations.length[CIString](Length, _.length).tap

  opaque type Email = CIString
  object Email:
    extension (self: Member.Email) def toCIString: CIString = self
    def unsafeFromCIString(value: CIString): Member.Email = value
    val validation: Validation[CIString, Member.Email] =
      val pattern = Pattern.compile(".+@.+")
      Validation.ask[CIString].map(_.toString).andThen(validations.matches(pattern)).tap

    given (using eq: Eq[CIString]): Eq[Member.Email] = eq

  opaque type Password = String
  object Password:
    extension (self: Member.Password) def toString: String = self
    def unsafeFromString(value: String): Member.Password = value
    val validation: Validation[String, Member.Password] = validations.minLength(6).tap

  opaque type Session = UUID
  object Session:
    extension (self: Member.Session) def toUUID: UUID = self
    def fromUUID(uuid: UUID): Member.Session = uuid

    given (using eq: Eq[UUID]): Eq[Member.Session] = eq

  final case class Create(email: Member.Email, password: Member.Password)

  final case class Summary(reference: Member.Reference, email: Member.Email)
