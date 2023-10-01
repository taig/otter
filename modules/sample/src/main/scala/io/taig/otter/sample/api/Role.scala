package io.taig.otter.sample.api

import cats.implicits.*
import io.taig.otter.sample.api.Role.Guest
import io.taig.otter.sample.data.{Librarian, Member}

enum Role:
  case Guest
  case Librarian
  case Member
  case Or[A <: Role, B <: Role](a: A, b: B)

  def ^(role: Role): this.type ^ role.type = Or(this, role)

  def toSet: Set[Role.Guest | Role.Librarian | Role.Member] = this match
    case Guest     => Set(Guest)
    case Librarian => Set(Librarian)
    case Member    => Set(Member)
    case Or(a, b)  => a.toSet ++ b.toSet

object Role:
  type Guest = Role.Guest.type
  type Librarian = Role.Librarian.type
  type Member = Role.Member.type

type ^[A <: Role, B <: Role] = Role.Or[A, B]

type Self[R] = R match
  case Role.Guest     => Unit
  case Role.Member    => Member
  case Role.Librarian => Librarian.Summary
  case Role.Guest ^ a => Option[Self[a]]
  case a ^ Role.Guest => Option[Self[a]]
  case a ^ b          => Self[a] | Self[b]
