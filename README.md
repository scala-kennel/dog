# dog

[![Build Status](https://travis-ci.org/pocketberserker/dog.svg?branch=master)](https://travis-ci.org/pocketberserker/dog)

**dog** is yet another testing framework for Scala.
This framework is inspired by [Persimmon](https://github.com/persimmon-projects/Persimmon).

## features

* composable test case and assertion
* [scalaprops](https://github.com/scalaprops/scalaprops) friendly

## latest stable version

```scala
testFrameworks += new TestFramework("dog.DogFramework")

libraryDependencies += "com.github.pocketberserker" %% "dog" % "0.1.4" % "test"
```

```scala
libraryDependencies += "com.github.pocketberserker" %% "dog-gen" % "0.1.4" % "test"
```

```scala
libraryDependencies += "com.github.pocketberserker" %% "dog-props" % "0.1.4" % "test"
```

or you can use [sbt plugin](https://github.com/pocketberserker/sbt-dog)

## Examples

see also [dog-examples](https://github.com/pocketberserker/dog-examples)

