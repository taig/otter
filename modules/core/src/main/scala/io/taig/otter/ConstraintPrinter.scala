package io.taig.otter

object ConstraintPrinter:
  def apply(constraint: Constraint): String = constraint match
    case Constraint.Type(name)                                      => s"type \"$name\""
    case Constraint.OneOf(values)                                   => s"oneOf [${values.toList.mkString(",")}]"
    case Constraint.Collection.MaxItems(reference)                  => s"maxItem $reference"
    case Constraint.Collection.MinItems(reference)                  => s"minItem $reference"
    case Constraint.Collection.UniqueItems                          => s"uniqueItems"
    case Constraint.Object.MaxProperties(reference)                 => s"maxProperties $reference"
    case Constraint.Object.MinProperties(reference)                 => s"minProperties $reference"
    case Constraint.Primitive.Matches(pattern)                      => s"matches \"$pattern\""
    case Constraint.Primitive.Maximum(Comparison(reference, true))  => s"lt $reference"
    case Constraint.Primitive.Maximum(Comparison(reference, false)) => s"lteq $reference"
    case Constraint.Primitive.Minimum(Comparison(reference, true))  => s"gt $reference"
    case Constraint.Primitive.Minimum(Comparison(reference, false)) => s"gteq $reference"
    case Constraint.Primitive.MaxLength(reference)                  => s"maxLength $reference"
    case Constraint.Primitive.MinLength(reference)                  => s"minLength $reference"
    case Constraint.Primitive.Multiple(reference)                   => s"multiple $reference"
