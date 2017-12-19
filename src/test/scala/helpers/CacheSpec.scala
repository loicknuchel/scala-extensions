package helpers

import org.scalatest.{FunSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try

class CacheSpec extends FunSpec with Matchers {
  describe("memoize") {
    it("should not compute function twice for the same input") {
      var x = 0
      val memoized: String => String = Cache.memoize { (in: String) =>
        x += 1
        in
      }
      x shouldBe 0
      memoized("test")
      x shouldBe 1
      memoized("test")
      x shouldBe 1
      memoized("toto")
      x shouldBe 2
    }
    it("should keep memoized value only for a limited time") {
      var x = 0
      val memoized: String => String = Cache.memoize(100.millis) { (in: String) =>
        x += 1
        in
      }
      x shouldBe 0
      memoized("test")
      x shouldBe 1
      memoized("test")
      x shouldBe 1
      memoized("toto")
      x shouldBe 2
      Thread.sleep(200)
      memoized("test")
      x shouldBe 3
    }
  }
  describe("memoizeAsync") {
    it("should memoize async actions") {
      var x = 0
      val memoized: String => Future[String] = Cache.memoizeAsync(100.millis) { (in: String) =>
        Future {
          x += 1
          in
        }
      }
      x shouldBe 0
      Await.result(memoized("test"), Duration.Inf)
      x shouldBe 1
      Await.result(memoized("test"), Duration.Inf)
      x shouldBe 1
      Await.result(memoized("toto"), Duration.Inf)
      x shouldBe 2
    }
    it("should not memoize failed results") {
      var x = 0
      val memoized: String => Future[String] = Cache.memoizeAsync(100.millis) { (in: String) =>
        Future {
          x += 1
          throw new Exception("err")
        }
      }
      x shouldBe 0
      Try(Await.result(memoized("test"), Duration.Inf))
      x shouldBe 1
      Try(Await.result(memoized("test"), Duration.Inf))
      x shouldBe 2
    }
    it("should not launch multiple computations even if result is not yet received") {
      var x = 0
      val memoized: String => Future[String] = Cache.memoizeAsync(100.millis) { (in: String) =>
        Future {
          Thread.sleep(10)
          x += 1
          in
        }
      }
      x shouldBe 0
      memoized("test")
      x shouldBe 0
      Await.result(memoized("test"), Duration.Inf)
      x shouldBe 1
      Thread.sleep(20)
      x shouldBe 1
    }
  }
  describe("memoizeAsyncOpt") {
    it("should memoize async actions with result") {
      var x = 0
      val memoized: String => Future[Option[String]] = Cache.memoizeAsyncOpt(100.millis) { (in: String) =>
        Future {
          x += 1
          Some(in)
        }
      }
      x shouldBe 0
      Await.result(memoized("test"), Duration.Inf)
      x shouldBe 1
      Await.result(memoized("test"), Duration.Inf)
      x shouldBe 1
      Await.result(memoized("toto"), Duration.Inf)
      x shouldBe 2
    }
    it("should not memoize failed results") {
      var x = 0
      val memoized: String => Future[Option[String]] = Cache.memoizeAsyncOpt(100.millis) { (in: String) =>
        Future {
          x += 1
          throw new Exception("err")
        }
      }
      x shouldBe 0
      Try(Await.result(memoized("test"), Duration.Inf))
      x shouldBe 1
      Try(Await.result(memoized("test"), Duration.Inf))
      x shouldBe 2
    }
    it("should not memoize empty results") {
      var x = 0
      val memoized: String => Future[Option[String]] = Cache.memoizeAsyncOpt(100.millis) { (in: String) =>
        Future {
          x += 1
          None
        }
      }
      x shouldBe 0
      Try(Await.result(memoized("test"), Duration.Inf))
      x shouldBe 1
      Try(Await.result(memoized("test"), Duration.Inf))
      x shouldBe 2
    }
    it("should not launch multiple computations even if result is not yet received") {
      var x = 0
      val memoized: String => Future[Option[String]] = Cache.memoizeAsyncOpt(100.millis) { (in: String) =>
        Future {
          Thread.sleep(10)
          x += 1
          Some(in)
        }
      }
      x shouldBe 0
      memoized("test")
      x shouldBe 0
      Await.result(memoized("test"), Duration.Inf)
      x shouldBe 1
      Thread.sleep(20)
      x shouldBe 1
    }
  }
}
