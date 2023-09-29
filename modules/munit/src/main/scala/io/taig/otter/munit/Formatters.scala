package io.taig.otter.munit

import io.taig.otter.http.Endpoint

private[otter] object Formatters:
  def testMessage(endpoint: Endpoint[?, ?], description: String): String =
    val request = endpoint.request
    val coordinates = s"${request.method} ${request.url.print}"
    if description.isEmpty then coordinates else s"$coordinates: $description"
