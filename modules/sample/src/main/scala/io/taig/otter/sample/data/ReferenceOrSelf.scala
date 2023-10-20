package io.taig.otter.sample.data

enum ReferenceOrSelf[+A]:
  case Self
  case Reference(value: A)
