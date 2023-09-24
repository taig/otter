package io.taig.otter.sample

import cats.syntax.all.*
import io.taig.otter.validation.Validation
import io.taig.otter.validation.validations
import org.typelevel.ci.CIString

import java.util.regex.Pattern

final case class Administrator(reference: Administrator.Reference, email: Administrator.Email)

object Administrator:
  opaque type Reference = CIString
  object Reference:
    val Length = 8
    extension (self: Administrator.Reference) def toCIString: CIString = self
    def unsafeFromCIString(value: CIString): Administrator.Reference = value
    val validation: Validation[CIString, Administrator.Reference] = validations.length[CIString](Length, _.length).tap

  opaque type Email = CIString
  object Email:
    extension (self: Administrator.Email) def toCIString: CIString = self
    def unsafeFromCIString(value: CIString): Administrator.Email = value
    val validation: Validation[CIString, Administrator.Email] =
      val pattern = Pattern.compile(".+@.+")
      Validation.ask[CIString].map(_.toString).andThen(validations.matches(pattern)).tap
