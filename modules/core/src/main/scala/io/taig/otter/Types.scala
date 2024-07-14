package io.taig.otter

import io.taig.otter as Base

trait Types:
  export Base.{
    Constraint,
    Dictionary,
    Enumeration,
    Primitive,
    Product,
    Record,
    Schema,
    SchemaValidation,
    Sum,
    Type,
    Union,
    Value
  }
