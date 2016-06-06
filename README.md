# dog

[![Build Status](https://travis-ci.org/scala-kennel/dog.svg?branch=master)](https://travis-ci.org/scala-kennel/dog)

**dog** is yet another testing framework for Scala.
This framework is inspired by [Persimmon](https://github.com/persimmon-projects/Persimmon).

## features

* composable test case and assertion
* [scalaprops](https://github.com/scalaprops/scalaprops) friendly

## latest stable version

```scala
testFrameworks += new TestFramework("dog.DogFramework")

libraryDependencies += "com.github.pocketberserker" %% "dog" % "0.3.0" % "test"
```

```scala
libraryDependencies += "com.github.pocketberserker" %% "dog-gen" % "0.3.0" % "test"
```

```scala
libraryDependencies += "com.github.pocketberserker" %% "dog-props" % "0.3.0" % "test"
```

or you can use [sbt plugin](https://github.com/scala-kennel/sbt-dog)

## Examples

see also [dog-examples](https://github.com/scala-kennel/dog-examples)

## Related projects

* [dog-autodoc](https://github.com/scala-kennel/dog-autodoc)
    * Generate documentation from request-dog

