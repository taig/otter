package io.taig.otter.sample.app.route.librarian.librarians.reference

import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import io.taig.otter.http.Route
import io.taig.otter.sample.api.endpoint
import io.taig.otter.sample.api.schema.librarian.ErrorApiSchema.LibrarianReferenceUnknown
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema

def get(librarians: Ref[IO, List[LibrarianApiSchema]]) = Route(
  endpoint.librarian.librarians.reference.get,
  implementation = (reference, _) =>
    librarians.get.map(_.find(_.reference === reference)).map(_.toRight(LibrarianReferenceUnknown(reference)))
)
