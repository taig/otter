package io.taig.otter.sample.api

opaque type User = Librarian.Summary | Member

object User:
  extension (self: User)
    def toRole: Role = self match
      case _: Librarian.Summary => Role.Librarian
      case _: Member            => Role.Member

  def apply(user: Librarian.Summary | Member): User = user
