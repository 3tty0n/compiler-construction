object Base {
  class MyError(message: String = null, cause: Throwable = null)
    extends RuntimeException(message: String, cause: Throwable)

  def error() = throw new MyError("error")

  type Var = String
}
