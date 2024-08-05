package io.taig.otter.sample

import cats.Eq
import java.util.UUID

opaque type Session = UUID
object Session:
  extension (self: Session) def toUUID: UUID = self

  def apply(uuid: UUID): Session = uuid

  given (using eq: Eq[UUID]): Eq[Session] = eq
