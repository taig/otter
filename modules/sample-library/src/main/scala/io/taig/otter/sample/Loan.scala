package io.taig.otter.sample

import java.time.LocalDate
import java.time.Period
import java.util.UUID

/** A book, in somebody's hands, until a date.
  *
  * `period` is a `java.time.Period` rather than a number of days, which is the case `core-java-time` is here for: "six
  * months" and "a hundred and eighty days" are not the same span, and a wire format that could only say the second
  * would make every February wrong. It is carried as ISO-8601 text, which is what the module's primitives are.
  */
final case class Loan(
    reference: UUID,
    isbn: Isbn,
    member: UUID,
    borrowed: LocalDate,
    period: Period,
    due: LocalDate
)

object Loan:
  /** What a caller sends to borrow one.
    *
    * `period` may be left out, and is then the member's own -- a default that the *server* fills in and a caller
    * therefore need not know, which is exactly the asymmetry `.optional(default)` describes and the reason the server
    * and client documents for this endpoint differ.
    */
  final case class Request(isbn: Isbn, period: Option[Period])
