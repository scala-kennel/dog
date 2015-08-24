package dog

object ParameterizeTest extends Dog {

  val returnSampleValue = Parameterize((i: Int) => {
    val target = for {
      a <- TestCase.ok(i)
    } yield a
    Assert.equal(TestResult(i), target.run(()))
  })
}
