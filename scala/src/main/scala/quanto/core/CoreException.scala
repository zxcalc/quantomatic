package quanto.core

class CoreException(msg : String = null, cause: Throwable = null)
extends Exception(msg, cause)

class CoreProtocolException(msg : String = null, cause: Throwable = null)
extends CoreException(msg, cause)

class CoreUserExcepton(msg : String = null, cause: Throwable = null)
extends CoreException(msg, cause)

