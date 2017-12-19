package helpers

import scala.language.reflectiveCalls
import scala.util.{Failure, Success, Try}

object Helpers {
  def using[Resource <: {def close()}, A](resource: Resource)(block: Resource => A): A = {
    try {
      block(resource)
    } finally {
      if (resource != null) resource.close()
    }
  }

  def usingSafe[Resource <: {def close()}, A](resource: => Resource)(block: Resource => A): Try[A] = {
    Try(resource) match {
      case Success(r) if r != null =>
        val res = Try(block(r))
        r.close()
        res
      case Failure(e) => Failure(e)
    }
  }
}
