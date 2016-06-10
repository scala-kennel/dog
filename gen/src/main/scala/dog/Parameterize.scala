package dog

import scalaprops.Gen

object Parameterize {

  def apply[A0, B](f: A0 => TestCaseAp[B])(implicit G0: Gen[A0]): TestCaseAp[B] =
    f(G0.sample())

  def apply[A0, A1, B](f: (A0, A1) => TestCaseAp[B])(implicit G0: Gen[A0], G1: Gen[A1]): TestCaseAp[B] =
    f(G0.sample(), G1.sample())

  def apply[A0, A1, A2, B](f: (A0, A1, A2) => TestCaseAp[B])(implicit G0: Gen[A0], G1: Gen[A1], G2: Gen[A2]): TestCaseAp[B] =
    f(G0.sample(), G1.sample(), G2.sample())

  def apply[A0, A1, A2, A3, B](f: (A0, A1, A2, A3) => TestCaseAp[B])
    (implicit G0: Gen[A0], G1: Gen[A1], G2: Gen[A2], G3: Gen[A3]): TestCaseAp[B] =
    f(G0.sample(), G1.sample(), G2.sample(), G3.sample())

  def apply[A0, B](f: A0 => TestCase[B])(implicit G0: Gen[A0]): TestCase[B] =
    f(G0.sample())

  def apply[A0, A1, B](f: (A0, A1) => TestCase[B])(implicit G0: Gen[A0], G1: Gen[A1]): TestCase[B] =
    f(G0.sample(), G1.sample())

  def apply[A0, A1, A2, B](f: (A0, A1, A2) => TestCase[B])(implicit G0: Gen[A0], G1: Gen[A1], G2: Gen[A2]): TestCase[B] =
    f(G0.sample(), G1.sample(), G2.sample())

  def apply[A0, A1, A2, A3, B](f: (A0, A1, A2, A3) => TestCase[B])
    (implicit G0: Gen[A0], G1: Gen[A1], G2: Gen[A2], G3: Gen[A3]): TestCase[B] =
    f(G0.sample(), G1.sample(), G2.sample(), G3.sample())
}
