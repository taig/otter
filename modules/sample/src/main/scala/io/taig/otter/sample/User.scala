package io.taig.otter.sample

import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.validation.Validation
import io.taig.otter.validation.validations.*

final case class User(name: User.Name, age: User.Age, gender: Option[User.Gender], props: Json)

object User:
  opaque type Name = String
  object Name:
    extension (self: User.Name) def toString: String = self
    def unsafeFromString(value: String): User.Name = value
    val validation: Validation[String, User.Name] = (minLength(1) *> maxLength(80)).tap

  opaque type Age = Int
  object Age:
    extension (self: User.Age) def toInt: Int = self
    def unsafeFromInt(value: Int): User.Age = value
    val validation: Validation[Int, User.Age] = (minimum(18) *> maximum(99)).tap

  enum Gender:
    case Male
    case Female
    case ApacheHelicopter
