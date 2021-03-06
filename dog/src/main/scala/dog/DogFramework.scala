package dog

import sbt.testing.{Fingerprint, Framework}

class DogFramework extends Framework {

  override def name() = "Dog"

  override def fingerprints() = Array[Fingerprint](DogFingerprint)

  override def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader) =
    new DogRunner(args, remoteArgs, testClassLoader, DogRunner.taskdef2task)

  def slaveRunner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader, send: String => Unit) =
    runner(args, remoteArgs, testClassLoader)
}
