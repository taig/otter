package io.taig.otter.sample.api

import io.taig.otter.sample.data.Librarian
import io.taig.otter.sample.data.Member

opaque type User = Librarian.Summary | Member

object User:
  extension (self: User)
    def toRole: Role = self match
      case _: Librarian.Summary => Role.Librarian
      case _: Member            => Role.Member

  def apply(user: Librarian.Summary | Member): User = user
