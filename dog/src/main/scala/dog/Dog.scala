package dog

trait Dog {

  def param: Param = Param.default

  def listener: DogListener = DogListener.default
}
