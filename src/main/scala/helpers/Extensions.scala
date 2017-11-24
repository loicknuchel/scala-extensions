package helpers

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Extensions {

  implicit class SeqExtension[A](val elt: Seq[A]) extends AnyVal {
    def duplicates: Seq[A] =
      elt.diff(elt.distinct).distinct
  }

  implicit class OptionExtension[A](val elt: Option[A]) extends AnyVal {
    def toTry(e: => Throwable): Try[A] = elt match {
      case Some(v) => Success(v)
      case None => Failure(e)
    }

    def toFuture(e: => Throwable): Future[A] = elt match {
      case Some(v) => Future.successful(v)
      case None => Future.failed(e)
    }

    def toEither[E](e: => E): Either[E, A] = elt match {
      case Some(v) => Right(v)
      case None => Left(e)
    }
  }

  implicit class TryExtension[A](val elt: Try[A]) extends AnyVal {
    def toFuture: Future[A] = elt match {
      case Success(v) => Future.successful(v)
      case Failure(e) => Future.failed(e)
    }

    def toEither[E](f: Throwable => E): Either[E, A] = elt match {
      case Success(v) => Right(v)
      case Failure(e) => Left(f(e))
    }

    def mapError(f: Throwable => Throwable): Try[A] = elt match {
      case Success(v) => Success(v)
      case Failure(e) => Failure(f(e))
    }
  }

  implicit class FutureExtension[A](val elt: Future[A]) extends AnyVal {
    def await: Try[A] =
      await(Duration.Inf)

    def await(atMost: Duration = Duration.Inf): Try[A] =
      Try(Await.result(elt, atMost))

    def failWithOption(implicit executor: ExecutionContext): Future[Option[A]] =
      elt.map(Some(_)).recover { case _ => None }
  }

  implicit class EitherExtension[E, A](val elt: Either[E, A]) extends AnyVal {
    def get: A = elt match {
      case Right(v) => v
      case Left(e) => throw new NoSuchElementException(e.toString)
    }
  }

  implicit class SeqTryExtension[A](val elt: Seq[Try[A]]) extends AnyVal {
    def partition(): (Seq[Throwable], Seq[A]) = {
      val (errors, values) = elt.partition(_.isFailure)
      (errors.collect { case Failure(e) => e }, values.collect { case Success(v) => v })
    }

    def sequence: Try[Seq[A]] =
      Try(elt.map(_.get))

    def sequenceEither: Either[Seq[Throwable], Seq[A]] = {
      val (errors, values) = partition()
      if (errors.nonEmpty) Left(errors) else Right(values)
    }
  }

  implicit class SeqFutureExtension[A](val elt: Seq[Future[A]]) extends AnyVal {
    def sequence(implicit executor: ExecutionContext): Future[Seq[A]] =
      Future.sequence(elt)
  }

  implicit class SeqEitherExtension[E, A](val elt: Seq[Either[E, A]]) extends AnyVal {
    // parenthesis are needed to remove ambiguous calls with standard API
    def partition(): (Seq[E], Seq[A]) = {
      val (left, right) = elt.partition(_.isLeft)
      (left.collect { case Left(e) => e }, right.collect { case Right(a) => a })
    }

    def sequence: Either[Seq[E], Seq[A]] = {
      val (left, right) = partition()
      if (left.nonEmpty) Left(left) else Right(right)
    }

    def sequence(op: (E, E) => E): Either[E, Seq[A]] = sequence match {
      case Right(v) => Right(v)
      case Left(e) => Left(e.reduce(op))
    }
  }

  implicit class OptionTryExtension[A](val elt: Option[Try[A]]) extends AnyVal {
    def sequence: Try[Option[A]] = elt match {
      case Some(v) => v.map(Some(_))
      case None => Success(None)
    }
  }

  implicit class OptionFutureExtension[A](val elt: Option[Future[A]]) extends AnyVal {
    def sequence(implicit executor: ExecutionContext): Future[Option[A]] = elt match {
      case Some(v) => v.map(Some(_))
      case None => Future.successful(None)
    }
  }

  implicit class OptionEitherExtension[E, A](val elt: Option[Either[E, A]]) extends AnyVal {
    def sequence: Either[E, Option[A]] = elt match {
      case Some(v) => v.right.map(Some(_))
      case None => Right(None)
    }
  }

  implicit class EitherThrowableExtension[A](val elt: Either[Throwable, A]) extends AnyVal {
    def toFuture: Future[A] = elt match {
      case Right(v) => Future.successful(v)
      case Left(e) => Future.failed(e)
    }
  }

}
