package ducttape.util;

/**
 * Some bash code returned nonzero. That's bad.
 */
class BashException(msg: String) extends Exception(msg)
