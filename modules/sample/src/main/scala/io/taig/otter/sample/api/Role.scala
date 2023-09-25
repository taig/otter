package io.taig.otter.sample.api

import cats.Order
import cats.implicits.*

import scala.collection.immutable.SortedSet

type ^[+A, +B] = Either[A, B]

sealed abstract class Roles[A]:
  self =>
  def roles: SortedSet[Role]

  final def ^[B <: Role & Singleton](role: Roles[B]): Roles[A ^ B] = new Roles[A ^ B]:
    override def roles: SortedSet[Role] = self.roles ++ role.roles

object Roles:
  def apply[A <: Singleton & Role](role: A): Roles[A] = new Roles[A]:
    override def roles: SortedSet[Role] = SortedSet(role)

enum Role:
  case Guest
  case Member
  case Librarian

object Role:
  type Librarian = Role.Librarian.type
  type Member = Role.Member.type
  type Guest = Role.Guest.type

  val librarian: Roles[Role.Librarian] = Roles(Role.Librarian)
  val member: Roles[Role.Member] = Roles(Role.Member)
  val guest: Roles[Role.Guest] = Roles(Role.Guest)

  given Order[Role] = Order.by:
    case Guest     => 1
    case Member    => 2
    case Librarian => 3
