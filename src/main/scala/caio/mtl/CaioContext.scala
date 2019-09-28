package caio.mtl

import caio.{Caio, EventLog}

trait CaioContext extends Context[Caio, EventLog, Throwable] {
  def apply[C]: EnvironmentContext[Caio, EventLog, Throwable, C] =

}
