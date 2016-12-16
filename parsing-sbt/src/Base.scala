object Base {

  class UnboundValidException(
    message: String = null,
    cause: Throwable = null
  ) extends RuntimeException(message: String, cause: Throwable)

  def error(
    message: String = null,
    cause: Throwable = null
  ) = throw new UnboundValidException(message, cause)

  type Var = String

}
