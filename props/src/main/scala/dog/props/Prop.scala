package dog
package props

import scalaprops.{Property, Gen, Param => SParam, Shrink, AsProperty}

object Prop {

  def forAll[A1](f: A1 => Boolean)(implicit param: SParam, A1: Gen[A1]): TestCase[A1] =
    Property.forAllS(f)(A1, Shrink.empty)
      .toTestCase(param)
      .flatMapK(_ => TestResult(A1.sample()))

  def forAll[A1, A2](f: (A1, A2) => Boolean)(implicit param: SParam,
    A1: Gen[A1], A2: Gen[A2]): TestCase[(A1, A2)] =
    Property.forAllS(f)(A1, A2, Shrink.empty, Shrink.empty)
      .toTestCase(param)
      .flatMapK(_ => TestResult((A1.sample(), A2.sample())))

  def forAll[A1, A2, A3](f: (A1, A2, A3) => Boolean)(implicit param: SParam,
    A1: Gen[A1], A2: Gen[A2], A3: Gen[A3]): TestCase[(A1, A2, A3)] =
    Property.forAllS(f)(A1, A2, A3, Shrink.empty, Shrink.empty, Shrink.empty)
      .toTestCase(param)
      .flatMapK(_ => TestResult((A1.sample(), A2.sample(), A3.sample())))

  def forAll[A1, A2, A3, A4](f: (A1, A2, A3, A4) => Boolean)(implicit param: SParam,
    A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4]): TestCase[(A1, A2, A3, A4)] =
    Property.forAllS(f)(A1, A2, A3, A4, Shrink.empty, Shrink.empty, Shrink.empty, Shrink.empty)
      .toTestCase(param)
      .flatMapK(_ => TestResult((A1.sample(), A2.sample(), A3.sample(), A4.sample())))

  def forAll[A1, A2, A3, A4, A5](f: (A1, A2, A3, A4, A5) => Boolean)(implicit param: SParam,
    A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5]): TestCase[(A1, A2, A3, A4, A5)] =
    Property.forAllS(f)(A1, A2, A3, A4, A5, Shrink.empty, Shrink.empty, Shrink.empty, Shrink.empty, Shrink.empty)
      .toTestCase(param)
      .flatMapK(_ => TestResult((A1.sample(), A2.sample(), A3.sample(), A4.sample(), A5.sample())))

  def forAllG[A1](A1: Gen[A1])(f: A1 => Boolean)(implicit param: SParam): TestCase[A1] =
    Property.forAll[A1](f)(A1).toTestCase(param).flatMapK(_ => TestResult(A1.sample()))

  def forAllG[A1, A2](A1: Gen[A1], A2: Gen[A2])
    (f: (A1, A2) => Boolean)(implicit param: SParam): TestCase[(A1, A2)] =
    Property.forAll[A1, A2](f)(A1, A2).toTestCase(param).flatMapK(_ => TestResult((A1.sample(), A2.sample())))

  def forAllG[A1, A2, A3](A1: Gen[A1], A2: Gen[A2], A3: Gen[A3])
    (f: (A1, A2, A3) => Boolean)(implicit param: SParam): TestCase[(A1, A2, A3)] =
    Property.forAll[A1, A2, A3](f)(A1, A2, A3).toTestCase(param)
      .flatMapK(_ => TestResult((A1.sample(), A2.sample(), A3.sample())))

  def forAllG[A1, A2, A3, A4](A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4])
    (f: (A1, A2, A3, A4) => Boolean)(implicit param: SParam): TestCase[(A1, A2, A3, A4)] =
    Property.forAll[A1, A2, A3, A4](f)(A1, A2, A3, A4).toTestCase(param)
      .flatMapK(_ => TestResult((A1.sample(), A2.sample(), A3.sample(), A4.sample())))

  def forAllG[A1, A2, A3, A4, A5](A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5])
    (f: (A1, A2, A3, A4, A5) => Boolean)(implicit param: SParam): TestCase[(A1, A2, A3, A4, A5)] =
    Property.forAll[A1, A2, A3, A4, A5](f)(A1, A2, A3, A4, A5).toTestCase(param)
      .flatMapK(_ => TestResult((A1.sample(), A2.sample(), A3.sample(), A4.sample(), A5.sample())))

  def property1[R, A1](f: A1 => R, param: SParam = SParam.withCurrentTimeSeed())
    (implicit P: AsProperty[R], A1: Gen[A1], S1: Shrink[A1]): TestCase[A1] =
    Property.forall0(A1, S1)(a => P.asProperty(f(a))).toTestCase(param).flatMapK(_ => TestResult(A1.sample()))

  def property2[R, A1, A2](f: (A1, A2) => R, param: SParam = SParam.withCurrentTimeSeed())
    (implicit P: AsProperty[R], A1: Gen[A1], A2: Gen[A2], S1: Shrink[A1], S2: Shrink[A2]): TestCase[(A1, A2)] =
    Property.forall0(A1, S1)(a1 =>
      Property.forall0(A2, S2)(a2 =>
        P.asProperty(f(a1, a2))
      )
    ).toTestCase(param).flatMapK(_ => TestResult((A1.sample(), A2.sample())))

  def property3[R, A1, A2, A3](f: (A1, A2, A3) => R, param: SParam = SParam.withCurrentTimeSeed())
    (implicit P: AsProperty[R], A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], S1: Shrink[A1], S2: Shrink[A2], S3: Shrink[A3]): TestCase[(A1, A2, A3)] =
    Property.forall0(A1, S1)(a1 =>
      Property.forall0(A2, S2)(a2 =>
        Property.forall0(A3, S3)(a3 =>
          P.asProperty(f(a1, a2, a3))
        )
      )
    ).toTestCase(param).flatMapK(_ => TestResult((A1.sample(), A2.sample(), A3.sample())))

  def property[R: AsProperty, A1](f: A1 => R)(implicit param: SParam,
    A1: Gen[A1], S1: Shrink[A1]): TestCase[A1] =
    Prop.property1(f)

  def property[R: AsProperty, A1, A2](f: (A1, A2) => R)(implicit param: SParam,
    A1: Gen[A1], A2: Gen[A2], S1: Shrink[A1], S2: Shrink[A2]): TestCase[(A1, A2)] =
    Prop.property2(f)

  def property[R: AsProperty, A1, A2, A3](f: (A1, A2, A3) => R)(implicit param: SParam,
    A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], S1: Shrink[A1], S2: Shrink[A2], S3: Shrink[A3]): TestCase[(A1, A2, A3)] =
    Prop.property3(f)

  object NoShrink {
    def property1[R: AsProperty, A1](f: A1 => Property, param: SParam = SParam.withCurrentTimeSeed())
      (implicit A1: Gen[A1], S1: Shrink[A1] = Shrink.empty[A1]): TestCase[A1] =
      Prop.property1(f, param)

    def property2[R: AsProperty, A1, A2](f: (A1, A2) => Property)(implicit param: SParam,
      A1: Gen[A1], A2: Gen[A2], S1: Shrink[A1] = Shrink.empty[A1], S2: Shrink[A2] = Shrink.empty[A2]): TestCase[(A1, A2)] =
      Prop.property2(f, param)

    def property3[R: AsProperty, A1, A2, A3](f: (A1, A2, A3) => Property)(implicit param: SParam,
      A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], S1: Shrink[A1] = Shrink.empty[A1], S2: Shrink[A2] = Shrink.empty[A2], S3: Shrink[A3] = Shrink.empty[A3]): TestCase[(A1, A2, A3)] =
      Prop.property3(f, param)
  }

  def forAllS[A1](f: A1 => Boolean)(implicit param: SParam,
    A1: Gen[A1], S1: Shrink[A1]): TestCase[A1] =
    Property.forall0(A1, S1)(f.andThen(Property.prop))
      .toTestCase(param).flatMapK(_ => TestResult(A1.sample()))

  def forAllS[A1, A2](f: (A1, A2) => Boolean)(implicit param: SParam,
    A1: Gen[A1], A2: Gen[A2], S1: Shrink[A1], S2: Shrink[A2]): TestCase[(A1, A2)] =
    Property.forall0(A1, S1)(a1 =>
      Property.forall0(A2, S2)(a2 =>
        Property.prop(f(a1, a2))
      )
    ).toTestCase(param).flatMapK(_ => TestResult((A1.sample(), A2.sample())))

  def forAllS[A1, A2, A3](f: (A1, A2, A3) => Boolean)(implicit param: SParam,
    A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], S1: Shrink[A1], S2: Shrink[A2], S3: Shrink[A3]): TestCase[(A1, A2, A3)] =
    Property.forall0(A1, S1)(a1 =>
      Property.forall0(A2, S2)(a2 =>
        Property.forall0(A3, S3)(a3 =>
          Property.prop(f(a1, a2, a3))
        )
      )
    ).toTestCase(param).flatMapK(_ => TestResult((A1.sample(), A2.sample(), A3.sample())))

  def forAllS[A1, A2, A3, A4](f: (A1, A2, A3, A4) => Boolean)(implicit param: SParam,
    A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], S1: Shrink[A1], S2: Shrink[A2], S3: Shrink[A3], S4: Shrink[A4]): TestCase[(A1, A2, A3, A4)] =
    Property.forall0(A1, S1)(a1 =>
      Property.forall0(A2, S2)(a2 =>
        Property.forall0(A3, S3)(a3 =>
          Property.forall0(A4, S4)(a4 =>
            Property.prop(f(a1, a2, a3, a4))
          )
        )
      )
    ).toTestCase(param).flatMapK(_ => TestResult((A1.sample(), A2.sample(), A3.sample(), A4.sample())))

  def forAllS[A1, A2, A3, A4, A5](f: (A1, A2, A3, A4, A5) => Boolean)(implicit param: SParam,
    A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5:Gen[A5], S1: Shrink[A1], S2: Shrink[A2], S3: Shrink[A3], S4: Shrink[A4], S5: Shrink[A5]): TestCase[(A1, A2, A3, A4, A5)] =
    Property.forall0(A1, S1)(a1 =>
      Property.forall0(A2, S2)(a2 =>
        Property.forall0(A3, S3)(a3 =>
          Property.forall0(A4, S4)(a4 =>
            Property.forall0(A5, S5)(a5 =>
              Property.prop(f(a1, a2, a3, a4, a5))
            )
          )
        )
      )
    ).toTestCase(param).flatMapK(_ => TestResult((A1.sample(), A2.sample(), A3.sample(), A4.sample(), A5.sample())))
}
