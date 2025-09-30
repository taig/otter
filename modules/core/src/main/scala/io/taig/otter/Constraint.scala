package io.taig.otter

import io.taig.validation.Constraint as ValidationConstraint
import io.taig.data.Data
import cats.syntax.all.*
import cats.derived.*
import cats.Eq
import cats.Show

type Constraint = Constraint.Generic | ValidationConstraint

object Constraint:
  enum Generic derives Eq:
    case Equals(reference: Data)
    case OneOf(references: List[Data])
    case Required
    case Type(name: String)

    final override def toString: String = this match
      case Equals(reference) => show"_.equals $reference"
      case OneOf(references) => show"_.oneof ${references.mkString(",")}"
      case Required          => "_.required"
      case Type(name)        => show"_.type $name"

  object Generic:
    given Show[Generic] = Show.fromToString

  export ValidationConstraint.{Collection, Object, Primitive}

  given Eq[Constraint] with
    def eqv(x: Constraint, y: Constraint): Boolean = (x, y) match
      case (x: Constraint.Generic, y: Constraint.Generic)     => x === y
      case (x: ValidationConstraint, y: ValidationConstraint) => x === y
      case _                                                  => false

  given Show[Constraint] =
    case constraint: Constraint.Generic   => constraint.show
    case constraint: ValidationConstraint => constraint.show
