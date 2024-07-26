// package io.taig.otter

// import cats.data.NonEmptyChain
// import cats.syntax.all.*
// import cats.data.Chain

// abstract class Branches[+O <: Data, A]:
//   def branches: NonEmptyChain[Branch[?, ?]]

//   def decodeNested(identifier: String, value: String, data: Chain[(String, Data)]): Codec.Result[Option[A]]

//   def decodeMerged(identifier: String, data: Chain[(String, Data)]): Codec.Result[Option[A]]

//   def decodeKeyed(data: Chain[(String, Data)]): Codec.Result[Option[A]]

//   def decodeUntagged(data: Data): Codec.Result[Option[A]]

//   def encodeNested(identifier: String, value: String, a: A): Data.Object[Data.String | O]

//   def encodeKeyed(a: A): Data.Object[O]

//   def encodeMerged[P <: Data](identifier: String, a: A)(using O <:< Data.Object[P]): Data.Object[Data.String | P]

//   def encodeUntagged(a: A): O

// object Branches:
//   def apply[O <: Data, A](branch: Branch[O, A]): Branches[O, A] = new Branches[O, A]:
//     override def branches: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
//     override def decodeNested(identifier: String, value: String, data: Chain[(String, Data)]): Codec.Result[Option[A]] =
//       data.findWithRemainders { case (`identifier`, data) => data }
//       ???
//     override def decodeMerged(identifier: String, data: Chain[(String, Data)]): Codec.Result[Option[A]] = ???
//     override def decodeKeyed(data: Chain[(String, Data)]): Codec.Result[Option[A]] = ???
//     override def decodeUntagged(data: Data): Codec.Result[Option[A]] = branch.decode(data).toOption.valid
//     override def encodeNested(identifier: String, value: String, a: A): Data.Object[Data.String | O] =
//       Data.Object.of(value -> branch.encode(a), identifier -> Data.String(branch.name))
//     override def encodeMerged[P <: Data](identifier: String, a: A)(using
//         O <:< Data.Object[P]
//     ): Data.Object[Data.String | P] = branch.encode(a) ++ Data.Object.of(identifier -> Data.String(branch.name))
//     override def encodeKeyed(a: A): Data.Object[O] = Data.Object.one(branch.name, branch.encode(a))
//     override def encodeUntagged(a: A): O = branch.encode(a)
