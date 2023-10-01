package io.taig.otter.sample

import io.taig.otter.sample.api.{^, Role}
import io.taig.otter.sample.data.{Librarian, Member}

import java.util.UUID

object Playground {
  type Endpoint[R, I, O] = io.taig.otter.http.Endpoint[R, I, O]

  enum Role:
    case Member
    case Librarian
    case Guest
    case Or[A <: Role, B <: Role](a: A, b: B)

  object Role:
    type Member = Role.Member.type
    type Librarian = Role.Librarian.type
    type Guest = Role.Guest.type

  type ^[A <: Role, B <: Role] = Role.Or[A, B]

  type Self[R <: Role] = R match
    case Role.Member    => Member
    case Role.Librarian => Librarian.Summary
    case Role.Guest     => Unit
    case Role.Guest ^ a => Option[Self[a]]
    case a ^ Role.Guest => Option[Self[a]]
    case a ^ b          => Self[a] | Self[b]

  def test[R <: Role]: Self[R] = ???

  val a: Member = test[Role.Member]
  val b: Option[Member] = test[Role.Guest ^ Role.Member]
  val c: Member | Librarian.Summary = test[Role.Member ^ Role.Librarian]

  println(valueOf[Role.Librarian])
}
