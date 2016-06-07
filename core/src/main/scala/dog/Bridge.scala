package dog

trait Bridge[F[_]] {
  def toTestCase[A](value: => F[A]): TestCase[A]
}
