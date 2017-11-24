package helpers

import com.typesafe.config.ConfigFactory
import org.scalatest.{FunSpec, Matchers}

import scala.util.Try

class BetterConfigSpec extends FunSpec with Matchers {
  describe("BetterConfig") {
    it("should show the full path in error") {
      val config = BetterConfig(ConfigFactory.parseString(
        """user {
          |  name: test
          |}
        """.stripMargin))
      Try(config.getConfig("user").getString("name")).get shouldBe "test"
      Try(config.getConfig("user").getString("notfound")).failed.get.getMessage shouldBe "No String found at 'user.notfound'"
      Try(config.getString("user.notfound")).failed.get.getMessage shouldBe "No String found at 'user.notfound'"
      Try(config.getString("a.b")).failed.get.getMessage shouldBe "No String found at 'a.b'"
    }
    it("should show the array index in error full path") {
      val config = BetterConfig(ConfigFactory.parseString(
        """items: [
          |  {name: test, value: test}
          |  {name: test, value: test}
          |]
        """.stripMargin))
      Try(config.getConfigList("items")(1).getString("notfound")).failed.get.getMessage shouldBe "No String found at 'items.1.notfound'"
    }
    it("should show the context in errors when available") {
      val config = BetterConfig(ConfigFactory.empty, ctx = Some("context"))
      Try(config.getString("notfound")).failed.get.getMessage shouldBe "context: No String found at 'notfound'"
    }
    it("should show asked type in error") {
      val config = BetterConfig(ConfigFactory.empty)
      Try(config.getString("notfound")).failed.get.getMessage shouldBe "No String found at 'notfound'"
      Try(config.getInt("notfound")).failed.get.getMessage shouldBe "No Int found at 'notfound'"
      Try(config.getDouble("notfound")).failed.get.getMessage shouldBe "No Double found at 'notfound'"
      Try(config.getBoolean("notfound")).failed.get.getMessage shouldBe "No Boolean found at 'notfound'"
      Try(config.getConfig("notfound")).failed.get.getMessage shouldBe "No Config found at 'notfound'"
      Try(config.getStringList("notfound")).failed.get.getMessage shouldBe "No Seq[String] found at 'notfound'"
      Try(config.getIntList("notfound")).failed.get.getMessage shouldBe "No Seq[Int] found at 'notfound'"
      Try(config.getDoubleList("notfound")).failed.get.getMessage shouldBe "No Seq[Double] found at 'notfound'"
      Try(config.getBooleanList("notfound")).failed.get.getMessage shouldBe "No Seq[Boolean] found at 'notfound'"
      Try(config.getConfigList("notfound")).failed.get.getMessage shouldBe "No Seq[Config] found at 'notfound'"
    }
    it("should give all keys") {
      val config = BetterConfig(ConfigFactory.parseString(
        """user {
          | id: 1
          | name: test
          |}
        """.stripMargin))
      config.keys() shouldBe Set("user.id", "user.name")
    }
    it("should give only child keys") {
      val config = BetterConfig(ConfigFactory.parseString(
        """user {
          | id: 1
          | name: test
          |}
        """.stripMargin))
      config.childKeys() shouldBe Set("user")
    }
    it("should get a Map") {
      val configFull = BetterConfig(ConfigFactory.parseString(
        """keys {
          | toto: toto
          | test {
          |   name: test
          | }
          |}
        """.stripMargin))
      configFull.getValueMap("keys").keys shouldBe Set("toto", "test")

      val config = BetterConfig(ConfigFactory.parseString(
        """keys {
          | toto: toto
          | test: test
          |}
        """.stripMargin))
      config.getStringMap("keys") shouldBe Map("toto" -> "toto", "test" -> "test")
    }
    it("should add context to exceptions") {
      val config = BetterConfig(ConfigFactory.parseString("keys {toto: toto, val: 1}")).getConfig("keys")
      val err = Try(config.getString("toto").toInt).failed.get
      err.getMessage shouldBe """For input string: "toto""""
      config.addContext(err).getMessage shouldBe """NumberFormatException For input string: "toto" at 'keys'"""
    }
  }
}
