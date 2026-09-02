package io.taig.otter.component

import io.taig.otter.Reference
import io.taig.otter.codec.Encoder
import io.taig.otter.operation.FieldOperation
import io.taig.otter.operation.RecordOperation

trait RecordComponent[Bound[-_, +_], F[_[-w, +r] <: Bound[w, r], -_, +_], G[_[-w, +r] <: Bound[w, r], -_, +_]]:
  /** The empty record. It holds nothing, so its `S` is the bottom constructor and widens to any other. */
  def RNil(using
      F: RecordOperation[[w, r] =>> F[Nothing, w, r], [w, r] =>> G[Nothing, w, r]]
  ): F[Nothing, Unit, Unit] = F.empty

object RecordComponent:
  /** `K` is the type a name may be spelled with, which is a text primitive. A field's name is matched against the key
    * the document holds and reported in a violation path, both of which are a `String`, so a typed name is printed here
    * rather than carried into the node.
    */
  trait Field[Bound[-_, +_], K[-_, +_], F[_[-w, +r] <: Bound[w, r], -_, +_]](using key: Encoder[K, String]):
    def apply[S[-w, +r] <: Bound[w, r], W, R](name: String, schema: => S[W, R])(using
        F: FieldOperation[[w, r] =>> F[S, w, r], S]
    ): F[S, W, R] = F.lift(name, Reference.later(schema))

    /** The name's schema is only ever written, so its read side is left open and a [[PrimitiveComponent.Text.printer]]
      * names a field as readily as a round tripping schema does. It is taken by value rather than by name because,
      * unlike the schema the field holds, it is forced immediately and can never be recursive.
      */
    def apply[S[-w, +r] <: Bound[w, r], N, W, R](name: N, tpe: K[N, Any], schema: => S[W, R])(using
        F: FieldOperation[[w, r] =>> F[S, w, r], S]
    ): F[S, W, R] = F.lift(key.encode(tpe, name), Reference.later(schema))
