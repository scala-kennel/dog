package dog

import sbt.testing.SubclassFingerprint

private[dog] object DogFingerprint extends SubclassFingerprint {
  override def isModule: Boolean = true
  override def superclassName(): String = "dog.Dog"
  override def requireNoArgConstructor(): Boolean = true
}
