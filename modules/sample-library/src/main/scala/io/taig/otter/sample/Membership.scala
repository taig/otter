package io.taig.otter.sample

import java.time.Period

/** What a membership entitles somebody to.
  *
  * The loan period hangs off the case rather than being carried in the record, so the rule lives in one place and an
  * endpoint answering with a `Loan` never has to be told how long one lasts.
  */
enum Membership:
  case Standard
  case Student
  case Staff

  /** How long this membership may keep a book. */
  def loanPeriod: Period = this match
    case Membership.Standard => Period.ofWeeks(3)
    case Membership.Student  => Period.ofWeeks(6)
    case Membership.Staff    => Period.ofMonths(6)
