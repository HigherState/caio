package caio.std

class CaioDispatcherAlreadyClosedException extends Throwable {
  override def getMessage: String = "dispatcher already shutdown"
}