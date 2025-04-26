package io.taig.otter.sample

import org.typelevel.ci.*

import java.util.UUID

final case class Librarian(reference: UUID, email: CIString, password: String, session: Option[Session])

object Librarian:
  final case class Login(email: CIString, password: String)
