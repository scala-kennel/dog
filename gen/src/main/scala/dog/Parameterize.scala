package dog

import scalaprops.Gen

object Parameterize {

  def apply[F[_], A0, B](f: A0 => F[B])(implicit B: Bridge[F], G0: Gen[A0]): TestCase[B] =
    B.toTestCase(f(G0.sample()))

  def apply[F[_], A0, A1, B](f: (A0, A1) => F[B])(implicit B: Bridge[F], G0: Gen[A0], G1: Gen[A1]): TestCase[B] =
    B.toTestCase(f(G0.sample(), G1.sample()))

  def apply[F[_], A0, A1, A2, B](f: (A0, A1, A2) => F[B])(implicit B: Bridge[F], G0: Gen[A0], G1: Gen[A1], G2: Gen[A2]): TestCase[B] =
    B.toTestCase(f(G0.sample(), G1.sample(), G2.sample()))

  def apply[F[_], A0, A1, A2, A3, B](f: (A0, A1, A2, A3) => F[B])
    (implicit B: Bridge[F], G0: Gen[A0], G1: Gen[A1], G2: Gen[A2], G3: Gen[A3]): TestCase[B] =
    B.toTestCase(f(G0.sample(), G1.sample(), G2.sample(), G3.sample()))
}
