package io.taig.otter.openapi

import io.taig.otter as Plain
import cats.Id as Identity
import io.taig.otter.openapi as OpenApi
import io.taig.otter.Types
import cats.syntax.all.*

trait Dsl extends Types:
  final override type Schema[A] = Annotation[
    [_] =>> Plain.Schema[Annotation[Plain.Schema[*, ?], Metadata[Identity]], A],
    Metadata[Identity]
  ]

  override object Schema extends Schemas:
    override type Of[+Of, A] = Annotation[
      [_] =>> Plain.Schema[Of, Metadata[Identity]],
      Metadata[Identity]
    ]

  final override type Primitive[A] = Annotation[[_] =>> Plain.Primitive[A], Metadata.Primitive[Identity]]

  override object Collection extends Collections:
    final override type Reader[A] = Any

    override object Reader extends Readers:
      final override type Of[+Of, A]

    override object Writer extends Writers

  final override type Tuple[A] = Annotation[
    [_] =>> Plain.Tuple[Annotation[Plain.Schema[*, ?], Metadata[Identity]], A],
    Metadata.Tuple[Identity]
  ]

  override object Tuple extends Tuples
