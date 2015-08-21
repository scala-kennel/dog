import scalaz._

package object dog {

  type TestCase[A] = Kleisli[TestResult, Unit, A]
}
