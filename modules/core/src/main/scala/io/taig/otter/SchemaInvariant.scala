// package io.taig.otter

// import cats.Invariant
// import cats.data.Chain
// import io.taig.otter.validation.Constraint
// import io.taig.otter.validation.Validation

// trait SchemaInvariant[F[_], G[_]] extends Invariant[G]:
//   def constraints[A](fa: G[A]): Chain[Constraint[?]]
//   def ivalidate[A, V1, V2, B](fa: G[A])(validation: SchemaValidation[F, A, V1, V2, B])(f: B => A): G[B]
//   override def imap[A, B](fa: G[A])(f: A => B)(g: B => A): G[B] = ivalidate(fa)(Validation.lift(f))(g)

// object SchemaInvariant:
//   trait Ops[F[_], G[_], A] extends Invariant.Ops[G, A]:
//     override type TypeClassType <: SchemaInvariant[F, G]
//     val typeClassInstance: TypeClassType
//     def self: G[A]
//     def constrains: Chain[Constraint[?]] = typeClassInstance.constraints(self)
//     def ivalidate[V1, V2, B](validation: SchemaValidation[F, A, V1, V2, B])(f: B => A): G[B] =
//       typeClassInstance.ivalidate(self)(validation)(f)
//     def ivalidate[V1, V2](validation: SchemaValidation[F, A, V1, V2, Unit]): G[A] = ivalidate(validation.tap)(identity)
