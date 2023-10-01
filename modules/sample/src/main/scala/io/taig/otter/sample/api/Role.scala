package io.taig.otter.sample.api

import cats.Order
import cats.implicits.*
import io.taig.otter.sample.api.Role.Guest
import io.taig.otter.sample.data.{Librarian, Member}

import scala.collection.immutable.SortedSet

sealed abstract class Role extends Product with Serializable

object Role:
  case object Guest extends Role
  case object Librarian extends Role
  case object Member extends Role
  final case class Or[A <: Role, B <: Role](a: A, b: B) extends Role

// object Role:
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

trait Roles[R <: Role]:
  def toSet: Set[Role.Guest | Role.Librarian | Role.Member]

object Roles:
  inline def apply[R <: Role](using roles: Roles[R]): Roles[R] = roles

  def apply[R <: Role](roles: => Set[Role.Guest | Role.Librarian | Role.Member]): Roles[R] =
    new Roles[R] { override def toSet: Set[Guest | Role.Librarian | Role.Member] = roles }

  given Roles[Role.Guest] = Roles(Set(Role.Guest))
  given Roles[Role.Librarian] = Roles(Set(Role.Librarian))
  given Roles[Role.Member] = Roles(Set(Role.Member))
  given [A <: Role, B <: Role](using a: Roles[A], b: Roles[B]): Roles[A ^ B] = Roles(a.toSet ++ b.toSet)
