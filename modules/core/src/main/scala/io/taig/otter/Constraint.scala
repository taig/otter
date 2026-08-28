package io.taig.otter

import cats.Eq
import cats.Show
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.validation.Constraint as ValidationConstraint

type Constraint = Constraint.Generic | ValidationConstraint

object Constraint:
  /** Constraints that are not tied to a particular value domain. */
  enum Generic:
    case Equals(reference: Data)
    case OneOf(references: List[Data])
    case Required
    case Type(name: String)

    final override def toString: String = this match
      case Equals(reference) => show"*.equals $reference"
      case OneOf(references) => show"*.oneof ${references.mkString(",")}"
      case Required          => "*.required"
      case Type(name)        => show"*.type $name"

  object Generic:
    given Eq[Constraint.Generic]:
      override def eqv(x: Constraint.Generic, y: Constraint.Generic): Boolean = (x, y) match
        case (Equals(x), Equals(y)) => x == y
        case (OneOf(x), OneOf(y))   => x == y
        case (Required, Required)   => true
        case (Type(x), Type(y))     => x === y
        case _                      => false

    given Show[Constraint.Generic] = Show.fromToString

  export ValidationConstraint.{Collection, Object, Primitive}

  given Eq[Constraint]:
    override def eqv(x: Constraint, y: Constraint): Boolean = (x, y) match
      case (x: Constraint.Generic, y: Constraint.Generic)     => x === y
      case (x: ValidationConstraint, y: ValidationConstraint) => x === y
      case _                                                  => false

  given Show[Constraint] =
    case constraint: Constraint.Generic   => constraint.show
    case constraint: ValidationConstraint => constraint.show
