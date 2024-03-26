package io.taig.otter.openapi

import io.taig.otter as Plain

final case class Annotation[+S[a] <: Plain.Schema[a], +M[+_], A](self: S[A], metadata: M[Annotation[S, M, A]])
