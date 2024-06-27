package io.taig.otter

import io.taig.otter as Base

// What we need:
// - Invariant, Functor and Contravariant instances for each schema type
// - Functor and Contravaraint syntax should be available to Invariant instances
// - Additional ops depending on Schema type

trait Instances extends Instances1:
  given primitiveRequiredInvariant: PrimitiveInvariant[
    Primitive.Required,
    Primitive.Required.Reader,
    Primitive.Required.Writer,
    Primitive
  ] = ???

  given primitiveValidationInvariant: ValidationInvariant[Primitive, Constraint.Primitive]

  given unionInvariant: UnionInvariant[Union.Of, Union.Reader.Of, Union.Writer.Of, Schema.Of, Collection.Of]

trait Instances1 extends Instances2:
  given primitiveInvariant: PrimitiveInvariant[
    Primitive,
    Primitive.Reader,
    Primitive.Writer,
    Primitive
  ] = ???

trait Instances2 extends Types:
  given schemaInvariant: SchemaInvariant = ???
