package quanto.core

class CoreException(val message : String = null, val cause: Throwable = null)
extends Exception(message, cause)

class CoreProtocolException(override val message : String = null, override val cause: Throwable = null)
extends CoreException(message, cause)

class CoreUserException(override val message : String, val code: Int)
extends CoreException(message, null)

