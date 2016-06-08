package dog

import scalaprops.Gen

object Parameterize {

  def apply[A0, B](f: A0 => TestCasesAp[B])(implicit G0: Gen[A0]): TestCasesAp[B] =
    f(G0.sample())

  def apply[A0, A1, B](f: (A0, A1) => TestCasesAp[B])(implicit G0: Gen[A0], G1: Gen[A1]): TestCasesAp[B] =
    f(G0.sample(), G1.sample())

  def apply[A0, A1, A2, B](f: (A0, A1, A2) => TestCasesAp[B])(implicit G0: Gen[A0], G1: Gen[A1], G2: Gen[A2]): TestCasesAp[B] =
    f(G0.sample(), G1.sample(), G2.sample())

  def apply[A0, A1, A2, A3, B](f: (A0, A1, A2, A3) => TestCasesAp[B])
    (implicit G0: Gen[A0], G1: Gen[A1], G2: Gen[A2], G3: Gen[A3]): TestCasesAp[B] =
    f(G0.sample(), G1.sample(), G2.sample(), G3.sample())

  def apply[A0, B](f: A0 => TestCases[B])(implicit G0: Gen[A0]): TestCases[B] =
    f(G0.sample())

  def apply[A0, A1, B](f: (A0, A1) => TestCases[B])(implicit G0: Gen[A0], G1: Gen[A1]): TestCases[B] =
    f(G0.sample(), G1.sample())

  def apply[A0, A1, A2, B](f: (A0, A1, A2) => TestCases[B])(implicit G0: Gen[A0], G1: Gen[A1], G2: Gen[A2]): TestCases[B] =
    f(G0.sample(), G1.sample(), G2.sample())

  def apply[A0, A1, A2, A3, B](f: (A0, A1, A2, A3) => TestCases[B])
    (implicit G0: Gen[A0], G1: Gen[A1], G2: Gen[A2], G3: Gen[A3]): TestCases[B] =
    f(G0.sample(), G1.sample(), G2.sample(), G3.sample())
}
