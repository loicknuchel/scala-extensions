package helpers

import java.util.Date

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration

object Cache {
  def memoize[K, V](f: K => V): K => V = {
    val cache = new mutable.HashMap[K, V]()
    (key: K) => cache.getOrElseUpdate(key, f(key))
  }

  def memoize[K, V](delay: Duration)(f: K => V): K => V = {
    val cache = new mutable.HashMap[K, (V, Date)]()
    (key: K) =>
      cache.get(key)
        .collect { case (v, date) if notTooOld(date, delay) => v }
        .getOrElse {
          val v = f(key)
          cache.put(key, (v, new Date()))
          v
        }
  }

  def memoizeAsync[K, V](delay: Duration)(f: K => Future[V])(implicit ec: ExecutionContext): K => Future[V] = {
    val cache = new mutable.HashMap[K, (V, Date)]()
    val futCache = new mutable.HashMap[K, Future[V]]()
    (key: K) =>
      cache.get(key)
        .collect { case (v, date) if notTooOld(date, delay) => Future.successful(v) }
        .getOrElse {
          val fut = futCache.getOrElseUpdate(key, f(key))
          fut.onComplete(_ => futCache.remove(key))
          fut.map { v =>
            cache.put(key, (v, new Date()))
            v
          }
        }
  }

  def memoizeAsyncOpt[K, V](delay: Duration)(f: K => Future[Option[V]])(implicit ec: ExecutionContext): K => Future[Option[V]] = {
    val cache = new mutable.HashMap[K, (V, Date)]()
    val futCache = new mutable.HashMap[K, Future[Option[V]]]()
    (key: K) =>
      cache.get(key)
        .collect { case (v, date) if notTooOld(date, delay) => Future.successful(Some(v)) }
        .getOrElse {
          val fut = futCache.getOrElseUpdate(key, f(key))
          fut.onComplete(_ => futCache.remove(key))
          fut.map { vOpt =>
            vOpt.foreach(v => cache.put(key, (v, new Date())))
            vOpt
          }
        }
  }

  private def notTooOld(date: Date, delay: Duration): Boolean =
    new Date().getTime - date.getTime < delay.toMillis
}
