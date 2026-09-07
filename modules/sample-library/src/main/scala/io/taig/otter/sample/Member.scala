package io.taig.otter.sample

import org.typelevel.ci.*
import org.typelevel.ci.CIString

import java.time.Instant
import java.time.LocalDate
import java.util.UUID
import scala.collection.immutable.SortedMap

/** Somebody who may borrow a book.
  *
  * The email is a `CIString` and not a `String`, which is `core-case-insensitive`'s reason to exist: two members who
  * typed their address in different cases are one member, and a type that forgot that would leave every lookup
  * depending on the shift key. The schema is the text primitive with a conversion on top rather than a refinement, so
  * the value really is case insensitive rather than merely documented as such.
  */
final case class Member(
    reference: UUID,
    email: CIString,
    joined: Instant,
    membership: Membership,
    expires: LocalDate,
    fines: SortedMap[String, BigDecimal]
)

object Member:
  /** The members a fresh server starts with. */
  val Seed: List[Member] = List(
    Member(
      reference = UUID.fromString("6f2a5c1e-0b3d-4f7a-9c8e-1d2b3a4c5d6e"),
      email = ci"ada@otter.test",
      joined = Instant.parse("2021-03-04T09:15:00Z"),
      membership = Membership.Standard,
      expires = LocalDate.of(2027, 3, 4),
      fines = SortedMap("9780141439518" -> BigDecimal("2.50"))
    ),
    Member(
      reference = UUID.fromString("7a1b2c3d-4e5f-4061-8172-93a4b5c6d7e8"),
      email = ci"grace@otter.test",
      joined = Instant.parse("2019-11-30T17:45:00Z"),
      membership = Membership.Staff,
      expires = LocalDate.of(2030, 1, 1),
      fines = SortedMap.empty
    )
  )
