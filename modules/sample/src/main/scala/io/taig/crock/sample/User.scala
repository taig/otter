package io.taig.crock.sample

import cats.syntax.all.*
import io.taig.crock.validation.Validation
import io.taig.crock.validation.validations.*

final class User(name: String, age: Int)

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
