// package io.taig.otter.json.circe

// import io.circe.Json
// import io.taig.otter.Primitive
// import io.taig.otter.Type
// import io.circe.syntax.*
// import java.math.BigDecimal as JBigDecimal
// import java.math.BigInteger as JBigInteger
// import io.taig.otter.Plain

// object JsonPrimitiveEncoder:
//   def apply[A](schema: Plain.Primitive.Writer[A], a: A): Json = schema match
//     case Primitive.Required.Writer.Modify(self, f) => apply(self, f(a))
//     case Primitive.Required.Writer.Root(tpe)       => apply(tpe, a)
//     case Primitive.Writer.Modify(self, f)          => apply(self, f(a))
//     case Primitive.Writer.Optional(self)           => a.map(apply(self, _)).getOrElse(Json.Null)
//     case Primitive.Required(_, writer)             => apply(writer, a)
//     case Primitive.Optional(_, writer)             => apply(writer, a)

//   def apply[A](tpe: Type[A], a: A): Json = tpe match
//     case Type.BigDecimal => (a: JBigDecimal).asJson
//     case Type.BigInteger => (a: JBigInteger).asJson
//     case Type.Boolean    => (a: Boolean).asJson
//     case Type.Double     => (a: Double).asJson
//     case Type.Float      => (a: Float).asJson
//     case Type.Int        => (a: Int).asJson
//     case Type.Long       => (a: Long).asJson
//     case Type.String     => (a: String).asJson
