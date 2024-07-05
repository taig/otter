package io.taig.otter

def typeOf(tpe: Type[?]): String = tpe match
  case Type.BigDecimal | Type.BigInteger | Type.Double | Type.Float | Type.Int | Type.Long => "number"
  case Type.Boolean                                                                        => "boolean"
  case Type.String                                                                         => "string"
