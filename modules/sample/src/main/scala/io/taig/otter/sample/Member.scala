package io.taig.otter.sample

import cats.syntax.all.*
import io.taig.otter.validation.{validations, Validation}
import org.typelevel.ci.CIString

import java.util.regex.Pattern

final case class Member(reference: Member.Reference, email: Member.Email)

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
