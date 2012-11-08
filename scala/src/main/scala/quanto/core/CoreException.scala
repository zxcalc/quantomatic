package quanto.core

case class CoreException(message : String = null, cause: Throwable = null)
extends Exception(message, cause)

case class CoreProtocolException(override val message : String = null, override val  cause: Throwable = null)
extends CoreException(message, cause)

case class CoreUserException(override val message : String, val code: Int)
extends CoreException(message)

