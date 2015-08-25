package dog

import java.util.concurrent.TimeUnit

trait Dog {

  def param: Param = Param.default

  def listener: DogListener = DogListener.default

  implicit class ParamSyntax[A] private[dog](val self: TestCase[A]) {

    def timeout(n: Int, timeunit: TimeUnit): TestCase[A] =
      self.local(p => Param.timeout(n, timeunit)(p))
  }
}
