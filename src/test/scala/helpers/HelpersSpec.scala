package helpers

import org.scalatest.{FunSpec, Matchers}

import scala.util.Success

class HelpersSpec extends FunSpec with Matchers {

  class Ressource(c: () => Unit) {
    def close(): Unit = c()
  }

  describe("using") {
    it("should call close at the end") {
      var x = 0
      Helpers.using(new Ressource(() => x = x + 1))(_ => "a") shouldBe "a"
      x shouldBe 1
    }
    it("should call close if operation failed") {
      var x = 0
      an[Exception] should be thrownBy Helpers.using(new Ressource(() => x = x + 1))(_ => throw new Exception)
      x shouldBe 1
    }
    it("should not call close if resource creation failed") {
      var x = 0
      an[Exception] should be thrownBy Helpers.using({
        val r = new Ressource(() => x = x + 1)
        throw new Exception
        r
      })(_ => "a")
      x shouldBe 0
    }
  }

  describe("usingSafe") {
    it("should call close at the end") {
      var x = 0
      Helpers.usingSafe(new Ressource(() => x = x + 1))(_ => "a") shouldBe Success("a")
      x shouldBe 1
    }
    it("should call close if operation failed") {
      var x = 0
      Helpers.usingSafe(new Ressource(() => x = x + 1))(_ => throw new Exception).isFailure shouldBe true
      x shouldBe 1
    }
    it("should not call close if resource creation failed") {
      var x = 0
      Helpers.usingSafe({
        val r = new Ressource(() => x = x + 1)
        throw new Exception
        r
      })(_ => throw new Exception).isFailure shouldBe true
      x shouldBe 0
    }
  }
}
