package helpers

import helpers.Extensions._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class ExtensionsSpec extends FunSpec with Matchers with ScalaFutures {
  val err = new Exception("msg")
  val err2 = new Exception("msg2")

  describe("SeqExtension") {
    describe("duplicates") {
      it("should return duplicate keys") {
        Seq(1, 2, 3).duplicates shouldBe Seq()
        Seq(1, 2, 3, 3).duplicates shouldBe Seq(3)
        Seq("a", "b", "a", "b").duplicates shouldBe Seq("a", "b")
      }
      it("should not return multiple times the same key") {
        Seq(1, 1, 1, 2).duplicates shouldBe Seq(1)
      }
    }
  }
  describe("OptionExtension") {
    describe("toTry") {
      it("should transform Option to Try") {
        Some(1).toTry(err) shouldBe Success(1)
        None.toTry(err) shouldBe Failure(err)
      }
    }
    describe("toFuture") {
      it("should transform Option to Future") {
        Some(1).toFuture(err).value shouldBe Some(Success(1))
        None.toFuture(err).failed.value.get.get shouldBe err
      }
    }
    describe("toEither") {
      it("should transform Option to Either") {
        Some(1).toEither("msg") shouldBe Right(1)
        None.toEither("msg") shouldBe Left("msg")
      }
    }
  }
  describe("TryConverter") {
    describe("toFuture") {
      it("should transform Try to Future") {
        Success(1).toFuture.value shouldBe Some(Success(1))
        Failure(err).toFuture.failed.value.get.get shouldBe err
      }
    }
    describe("toEither") {
      it("should transform Try to Either") {
        Success(1).asEither(_.getMessage) shouldBe Right(1)
        Failure(err).asEither(_.getMessage) shouldBe Left("msg")
      }
    }
  }
  describe("FutureConverter") {
    describe("await") {
      it("should await a Future") {
        Future(1).await shouldBe Success(1)
        Future.successful(1).await shouldBe Success(1)
        Future.failed(err).await shouldBe Failure(err)
      }
    }
    describe("failWithOption") {
      it("should wrap result in Option") {
        whenReady(Future(1).failWithOption) { res =>
          res shouldBe Some(1)
        }
      }
      it("should recover a failed Future") {
        whenReady(Future[Int](throw err).failWithOption) { res =>
          res shouldBe None
        }
      }
    }
  }
  describe("EitherExtension") {
    describe("get") {
      it("should get the right value") {
        Right(1).get shouldBe 1
        an[NoSuchElementException] should be thrownBy Left(1).get
      }
    }
  }
  describe("SeqTryExtension") {
    describe("partition") {
      it("should partition values and errors") {
        val (errors, values) = Seq(Success(1), Failure(err), Success(2), Failure(err2)).partition()
        errors shouldBe Seq(err, err2)
        values shouldBe Seq(1, 2)
      }
    }
    describe("sequence") {
      it("should sequence Seq of Try") {
        Seq(Success(1), Success(2)).sequence shouldBe Success(Seq(1, 2))
        Seq(Success(1), Failure(err)).sequence shouldBe Failure(err)
        Seq(Success(1), Failure(err), Failure(err2)).sequence shouldBe Failure(err)
      }
    }
    describe("sequenceEither") {
      it("should sequence Seq of Try and return Eithers") {
        Seq(Success(1), Success(2)).sequenceEither shouldBe Right(Seq(1, 2))
        Seq(Success(1), Failure(err)).sequenceEither shouldBe Left(Seq(err))
        Seq(Success(1), Failure(err), Failure(err2)).sequenceEither shouldBe Left(Seq(err, err2))
      }
    }
  }
  describe("SeqFutureExtension") {
    describe("sequence") {
      it("should sequence Seq of Future") {
        whenReady(Seq(Future.successful(1), Future.successful(2)).sequence) { res =>
          res shouldBe Seq(1, 2)
        }
        whenReady(Seq(Future.successful(1), Future.failed(err)).sequence.failed) { res =>
          res shouldBe err
        }
        whenReady(Seq(Future.successful(1), Future.failed(err), Future.failed(err2)).sequence.failed) { res =>
          res shouldBe err
        }
      }
    }
  }
  describe("SeqEitherExtension") {
    describe("partition") {
      it("should partition left and right elements") {
        Seq(Right(1), Right(2)).partition() shouldBe(Seq(), Seq(1, 2))
        Seq(Left("err"), Right(1)).partition() shouldBe(Seq("err"), Seq(1))
      }
    }
    describe("sequence") {
      it("should accumulate values in Either") {
        Seq(Right(1), Right(2)).sequence shouldBe Right(Seq(1, 2))
        Seq(Left("err"), Right(1)).sequence shouldBe Left(Seq("err"))
        Seq(Left("e1"), Left("e2")).sequence shouldBe Left(Seq("e1", "e2"))
        Seq(Left("e1"), Left("e2")).sequence((a, b) => a + b) shouldBe Left("e1e2")
      }
    }
  }
  describe("OptionTryConverter") {
    describe("sequence") {
      it("should sequence Option and Try") {
        Some(Success(1)).sequence shouldBe Success(Some(1))
        Some(Failure(err)).sequence shouldBe Failure(err)
        Option.empty[Try[Int]].sequence shouldBe Success(None)
      }
    }
  }
  describe("OptionFutureExtension") {
    describe("sequence") {
      it("should sequence Option and Future") {
        whenReady(Some(Future.successful(1)).sequence) { res =>
          res shouldBe Some(1)
        }
        whenReady(Some(Future.failed(err)).sequence.failed) { res =>
          res shouldBe err
        }
        whenReady(Option.empty[Future[Int]].sequence) { res =>
          res shouldBe None
        }
      }
    }
  }
  describe("OptionEitherConverter") {
    describe("sequence") {
      it("should sequence Option and Try") {
        Some(Right(1)).sequence shouldBe Right(Some(1))
        Some(Left("err")).sequence shouldBe Left("err")
        Option.empty[Either[String, Int]].sequence shouldBe Right(None)
      }
    }
  }
  describe("EitherThrowableExtension") {
    describe("toFuture") {
      it("should transform Either to Future") {
        whenReady(Right(1).toFuture) { res =>
          res shouldBe 1
        }
        whenReady(Left(err).toFuture.failed) { res =>
          res shouldBe err
        }
      }
    }
  }
}
