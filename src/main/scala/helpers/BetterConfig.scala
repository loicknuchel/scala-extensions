package helpers

import com.typesafe.config._

import scala.collection.JavaConverters._
import scala.util.Try

case class BetterConfig(config: Config, parentPath: Seq[String] = Seq(), ctx: Option[String] = None) {
  def getConfig(path: String): BetterConfig = get("Config", path, p => BetterConfig(config.getConfig(p), parentPath :+ p, ctx))
  def getConfigOrElse(path: String, default: => BetterConfig): BetterConfig = getOrElse(BetterConfig(config.getConfig(path), parentPath :+ path, ctx), default)
  def getConfigOrEmpty(path: String): BetterConfig = getConfigOrElse(path, BetterConfig(ConfigFactory.empty, parentPath :+ path, ctx))
  def getConfigOption(path: String): Option[BetterConfig] = getOption(BetterConfig(config.getConfig(path), parentPath :+ path, ctx))
  def getConfigList(path: String): Seq[BetterConfig] = Try(config.getConfigList(path).asScala.zipWithIndex.map { case (c, i) => BetterConfig(c, parentPath ++ Seq(path, i.toString), ctx) }).getOrElse(throw missingError(path, s"Seq[Config]"))
  def getConfigListOrElse(path: String, default: => Seq[BetterConfig]): Seq[BetterConfig] = Try(getConfigList(path)).getOrElse(default)
  def getConfigListOrEmpty(path: String): Seq[BetterConfig] = getConfigListOrElse(path, Seq())
  def getConfigListNonEmpty(path: String): Seq[BetterConfig] = nonEmpty(getConfigList(path), path)
  def getConfigMap(path: String): Map[String, BetterConfig] = getMap(path, (c, k) => c.getConfig(k))
  def getConfigMapOrElse(path: String, default: => Map[String, BetterConfig]): Map[String, BetterConfig] = Try(getConfigMap(path)).getOrElse(default)
  def getConfigMapOrEmpty(path: String): Map[String, BetterConfig] = getConfigMapOrElse(path, Map())
  def getConfigMapNonEmpty(path: String): Map[String, BetterConfig] = nonEmpty(getConfigMap(path), path)

  def getString(path: String): String = get("String", path, config.getString)
  def getStringOrElse(path: String, default: => String): String = getOrElse(config.getString(path), default)
  def getStringOrEmpty(path: String): String = getStringOrElse(path, "")
  def getStringOption(path: String): Option[String] = getOption(config.getString(path))
  def getStringList(path: String): Seq[String] = getList("String", path, config.getStringList)
  def getStringListOrElse(path: String, default: => Seq[String]): Seq[String] = Try(getStringList(path)).getOrElse(default)
  def getStringListOrEmpty(path: String): Seq[String] = getStringListOrElse(path, Seq())
  def getStringListNonEmpty(path: String): Seq[String] = nonEmpty(getStringList(path), path)
  def getStringMap(path: String): Map[String, String] = getMap(path, (c, k) => c.getString(k))
  def getStringMapOrElse(path: String, default: => Map[String, String]): Map[String, String] = Try(getStringMap(path)).getOrElse(default)
  def getStringMapOrEmpty(path: String): Map[String, String] = getStringMapOrElse(path, Map())
  def getStringMapNonEmpty(path: String): Map[String, String] = nonEmpty(getStringMap(path), path)

  def getBoolean(path: String): Boolean = get("Boolean", path, config.getBoolean)
  def getBooleanOrElse(path: String, default: => Boolean): Boolean = getOrElse(config.getBoolean(path), default)
  def getBooleanOrFalse(path: String): Boolean = getBooleanOrElse(path, default = false)
  def getBooleanOption(path: String): Option[Boolean] = getOption(config.getBoolean(path))
  def getBooleanList(path: String): Seq[Boolean] = getList("Boolean", path, config.getBooleanList).map(Boolean.unbox)
  def getBooleanListOrElse(path: String, default: => Seq[Boolean]): Seq[Boolean] = Try(getBooleanList(path)).getOrElse(default)
  def getBooleanListOrEmpty(path: String): Seq[Boolean] = getBooleanListOrElse(path, Seq())
  def getBooleanListNonEmpty(path: String): Seq[Boolean] = nonEmpty(getBooleanList(path), path)
  def getBooleanMap(path: String): Map[String, Boolean] = getMap(path, (c, k) => c.getBoolean(k))
  def getBooleanMapOrElse(path: String, default: => Map[String, Boolean]): Map[String, Boolean] = Try(getBooleanMap(path)).getOrElse(default)
  def getBooleanMapOrEmpty(path: String): Map[String, Boolean] = getBooleanMapOrElse(path, Map())
  def getBooleanMapNonEmpty(path: String): Map[String, Boolean] = nonEmpty(getBooleanMap(path), path)

  def getInt(path: String): Int = get("Int", path, config.getInt)
  def getIntOrElse(path: String, default: => Int): Int = getOrElse(config.getInt(path), default)
  def getIntOrZero(path: String): Int = getIntOrElse(path, 0)
  def getIntOption(path: String): Option[Int] = getOption(config.getInt(path))
  def getIntList(path: String): Seq[Int] = getList("Int", path, config.getIntList).map(_.toInt)
  def getIntListOrElse(path: String, default: => Seq[Int]): Seq[Int] = Try(getIntList(path)).getOrElse(default)
  def getIntListOrEmpty(path: String): Seq[Int] = getIntListOrElse(path, Seq())
  def getIntListNonEmpty(path: String): Seq[Int] = nonEmpty(getIntList(path), path)
  def getIntMap(path: String): Map[String, Int] = getMap(path, (c, k) => c.getInt(k))
  def getIntMapOrElse(path: String, default: => Map[String, Int]): Map[String, Int] = Try(getIntMap(path)).getOrElse(default)
  def getIntMapOrEmpty(path: String): Map[String, Int] = getIntMapOrElse(path, Map())
  def getIntMapNonEmpty(path: String): Map[String, Int] = nonEmpty(getIntMap(path), path)

  def getDouble(path: String): Double = get("Double", path, config.getDouble)
  def getDoubleOrElse(path: String, default: => Double): Double = getOrElse(config.getDouble(path), default)
  def getDoubleOrZero(path: String): Double = getDoubleOrElse(path, 0)
  def getDoubleOption(path: String): Option[Double] = getOption(config.getDouble(path))
  def getDoubleList(path: String): Seq[Double] = getList("Double", path, config.getDoubleList).map(_.toDouble)
  def getDoubleListOrElse(path: String, default: => Seq[Double]): Seq[Double] = Try(getDoubleList(path)).getOrElse(default)
  def getDoubleListOrEmpty(path: String): Seq[Double] = getDoubleListOrElse(path, Seq())
  def getDoubleListNonEmpty(path: String): Seq[Double] = nonEmpty(getDoubleList(path), path)
  def getDoubleMap(path: String): Map[String, Double] = getMap(path, (c, k) => c.getDouble(k))
  def getDoubleMapOrElse(path: String, default: => Map[String, Double]): Map[String, Double] = Try(getDoubleMap(path)).getOrElse(default)
  def getDoubleMapOrEmpty(path: String): Map[String, Double] = getDoubleMapOrElse(path, Map())
  def getDoubleMapNonEmpty(path: String): Map[String, Double] = nonEmpty(getDoubleMap(path), path)

  def getValue(path: String): ConfigValue = get("ConfigValue", path, config.getValue)
  def getValueMap(path: String): Map[String, ConfigValue] = getMap(path, (c, k) => c.getValue(k))

  def entrySet(): Set[(String, ConfigValue)] =
    config.entrySet().asScala.map(e => (e.getKey, e.getValue)).toSet

  def keys(): Set[String] =
    config.entrySet().asScala.map(_.getKey).toSet

  def childKeys(): Set[String] =
    keys().groupBy(_.split("\\.").head).keys.toSet

  def withValue(key: String, value: String): BetterConfig =
    BetterConfig(config.withValue(key, ConfigValueFactory.fromAnyRef(value)))

  def withValue(key: String, value: Int): BetterConfig =
    BetterConfig(config.withValue(key, ConfigValueFactory.fromAnyRef(value)))

  def withValue(key: String, value: Double): BetterConfig =
    BetterConfig(config.withValue(key, ConfigValueFactory.fromAnyRef(value)))

  def withValue(key: String, value: ConfigObject): BetterConfig =
    BetterConfig(config.withValue(key, value))

  def withValue(key: String, value: ConfigValue): BetterConfig =
    BetterConfig(config.withValue(key, value))

  def withValue(key: String, value: Option[Any]): BetterConfig =
    if (value.isDefined) BetterConfig(config.withValue(key, ConfigValueFactory.fromAnyRef(value.get)))
    else this

  def withValue(key: String, value: Seq[Any]): BetterConfig =
    withValue(key, value, showEmpty = true)

  def withValue(key: String, value: Seq[Any], showEmpty: Boolean): BetterConfig =
    if (showEmpty || value.nonEmpty) BetterConfig(config.withValue(key, ConfigValueFactory.fromIterable(value.asJava)))
    else this

  def withValue(key: String, value: Map[String, Any]): BetterConfig =
    withValue(key, value, showEmpty = true)

  def withValue(key: String, value: Map[String, Any], showEmpty: Boolean): BetterConfig =
    if (showEmpty || value.nonEmpty) BetterConfig(config.withValue(key, ConfigValueFactory.fromMap(value.asJava)))
    else this

  def addContext(e: Throwable): Throwable = e match {
    case _: BetterConfigException => e
    case _ => BetterConfigException(ctx, parentPath, config, e.getClass.getSimpleName + " " + e.getMessage, Some(e))
  }

  private def get[T](typ: String, path: String, getValue: String => T): T = Try(getValue(path)).getOrElse(throw missingError(path, typ))
  private def getOrElse[T](value: => T, default: => T): T = Try(value).getOrElse(default)
  private def getOption[T](value: => T): Option[T] = Try(value).toOption
  private def getList[T](typ: String, path: String, getValue: String => java.util.List[T]): Seq[T] = Try(getValue(path).asScala).getOrElse(throw missingError(path, s"Seq[$typ]"))
  private def getMap[T](path: String, getValue: (BetterConfig, String) => T): Map[String, T] = {
    val c = get("Map", path, p => BetterConfig(config.getConfig(p), parentPath :+ p, ctx))
    c.childKeys().map(k => (k, getValue(c, k))).toMap
  }
  private def nonEmpty[T](result: Seq[T], path: String): Seq[T] = if (result.nonEmpty) result else throw emptyListError(path)
  private def nonEmpty[T](result: Map[String, T], path: String): Map[String, T] = if (result.nonEmpty) result else throw emptyListError(path)

  private def missingError(path: String, typ: String): Throwable = BetterConfigException(ctx, parentPath :+ path, config, s"No $typ found", None)
  private def emptyListError(path: String): Throwable = BetterConfigException(ctx, parentPath :+ path, config, s"Invalid value: List must not be empty", None)
}

object BetterConfig {
  def empty = BetterConfig(ConfigFactory.empty)
}

case class BetterConfigException(ctx: Option[String],
                                 path: Seq[String],
                                 config: Config,
                                 message: String,
                                 cause: Option[Throwable])
  extends RuntimeException(
    ctx.map(_ + ": ").getOrElse("") +
      message +
      path.headOption.map(_ => " at '" + path.mkString(".") + "'").getOrElse(""),
    cause.orNull)
