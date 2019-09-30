package caio.std

import caio.mtl.{Context, EnvironmentContext}
import caio.{Caio, EventLog}

trait CaioContext extends Context[Caio, EventLog, Throwable] {
  def apply[C]: EnvironmentContext[Caio, EventLog, Throwable, C]

}
