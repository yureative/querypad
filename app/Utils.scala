import play.api.Logger

package object utils {
  def using[A <: { def close() }, B](closeable: A)(f: A => B): B =
    try { f(closeable) } finally { closeable.close() }
  
  def deleteQuietly(file: java.io.File) {
    if (!file.delete()) {
      Logger.warn(s"failed to delete file. [$file]")
    }   
  }
}

