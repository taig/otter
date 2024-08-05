package io.taig.otter.sample

import java.util.UUID
import org.typelevel.ci.*

final case class Librarian(reference: UUID, email: CIString, password: String, session: Option[Session]):
  def toLibratianSummary: Librarian.Summary = Librarian.Summary(reference, email)

object Librarian:
  final case class Login(email: CIString, password: String)

  final case class Create(email: CIString, password: String)

  final case class Summary(reference: UUID, email: CIString)
