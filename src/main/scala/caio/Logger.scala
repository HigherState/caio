package caio

private[caio] object Logger {
  def reportFailure(e: Throwable): Unit =
    Thread.getDefaultUncaughtExceptionHandler match {
      case null => e.printStackTrace()
      case h    => h.uncaughtException(Thread.currentThread(), e)
    }
}
