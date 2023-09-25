package io.taig.otter.sample.api

import cats.Eq
import cats.syntax.all.*
import io.taig.otter.validation.Validation
import io.taig.otter.validation.validations
import org.typelevel.ci.CIString

import java.util.UUID
import java.util.regex.Pattern

final case class Librarian(
    reference: Librarian.Reference,
    email: Librarian.Email,
    password: Librarian.Password,
    session: Option[Librarian.Session]
):
  def toSummary: Librarian.Summary = Librarian.Summary(reference, email)

object Librarian:
  opaque type Reference = CIString
  object Reference:
    val Length = 8
    extension (self: Librarian.Reference) def toCIString: CIString = self
    def unsafeFromCIString(value: CIString): Librarian.Reference = value
    val validation: Validation[CIString, Librarian.Reference] = validations.length[CIString](Length, _.length).tap

  opaque type Email = CIString
  object Email:
    extension (self: Librarian.Email) def toCIString: CIString = self
    def unsafeFromCIString(value: CIString): Librarian.Email = value
    val validation: Validation[CIString, Librarian.Email] =
      val pattern = Pattern.compile(".+@.+")
      Validation.ask[CIString].map(_.toString).andThen(validations.matches(pattern)).tap

    given (using eq: Eq[CIString]): Eq[Librarian.Email] = eq

  opaque type Password = String
  object Password:
    extension (self: Librarian.Password) def toString = self
    def unsafeFromString(value: String): Librarian.Password = value
    val validation: Validation[String, Librarian.Password] = validations.minLength(6).tap

  opaque type Session = UUID
  object Session:
    extension (self: Librarian.Session) def toUUID: UUID = self
    def fromUUID(uuid: UUID): Librarian.Session = uuid

    given (using eq: Eq[UUID]): Eq[Librarian.Session] = eq

  final case class Create(email: Librarian.Email, password: Librarian.Password)

  final case class Summary(reference: Librarian.Reference, email: Librarian.Email)
