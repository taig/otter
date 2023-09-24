package io.taig.otter.sample

import cats.syntax.all.*
import io.taig.otter.validation.Validation
import io.taig.otter.validation.validations
import org.typelevel.ci.CIString

import java.util.regex.Pattern

final case class Librarian(reference: Librarian.Reference, email: Librarian.Email)

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

  final case class Create(email: Librarian.Email)
