// package io.taig.otter

// final case class Plain[A](
//     self: Collection[Plain, A] | Constant[Plain, A] | Dictionary[Plain.Key, Plain, A] | Enumeration[A] |
//       Optional[Plain, A] | Primitive[A] | Record[Plain, A] | Tuple[Plain, A] | Union[Plain, A]
// ) extends AnyVal

// object Plain:
//   final case class Key[A](self: Constant[Plain.Key, A] | Enumeration[A] | Primitive[A] | Union.Untagged[Plain.Key, A])
//       extends AnyVal
