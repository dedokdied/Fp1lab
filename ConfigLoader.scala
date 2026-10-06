import java.nio.charset.StandardCharsets
import java.nio.file.*
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

// файл - кладовщик программы, его задача это читать настройки из текстового файла и превращать их в обьекты

object ConfigLoader: // синглтон тк в программе это кроме как тут нигде не требуется
  val DefaultPath = "config.txt" // конфиг в этом файле

  // функция, которую мы вызываем в main-е, если файл есть, то используем, если нет, то создаем дефолтный
  def loadOrDefault(path: String): Config =
    if Files.exists(Paths.get(path)) then
      load(path)
    else
      val config = defaultConfig
      save(path, config)
      config

  // превращает текст из текстового конфига в обьект
  def load(path: String): Config =
    val lines = Files.readAllLines(Paths.get(path), StandardCharsets.UTF_8)
      .asScala
      .toVector
      .map(_.trim)
      .filterNot(l => l.isEmpty || l.startsWith("#"))
    // читаем все строки, убираем пустые и комментарии

    // обьявляем значения по умолчанию
    var logFile = "anonymizer.log"
    var reportDirectory = "reports"
    var outputDirectory = "anonymized"
    var defaultPolicy = "strict"
    var overwriteExistingFiles = false
    var supportedExtensions = Seq(".txt", ".log", ".md", ".csv", ".json", ".xml")

    // тут собираем политики во время парсинга
    val policies = ArrayBuffer[Policy]()
    var i = 0

    // проходим по файлу построчно
    while i < lines.size do
      val line = lines(i)

      // если строка начинает со слова политика, то начался блок политики
      if line.startsWith("policy ") then
        val policyName = line.substring("policy ".length).trim
        i += 1

        var description = ""
        var includes = Seq.empty[String]
        val rules = ArrayBuffer[Rule]()
        var inPolicy = true

        // читаем все, что внутри политики, пока не видим "end"
        while inPolicy && i < lines.size do
          val policyLine = lines(i)
          if policyLine == "end" then
            inPolicy = false
            i += 1
          else if policyLine.startsWith("description=") then
            description = afterEquals(policyLine)
            i += 1
          else if policyLine.startsWith("includes=") then
            includes = afterEquals(policyLine)
              .split(",")
              .map(_.trim)
              .filter(_.nonEmpty)
              .toSeq
            i += 1
          else if policyLine.startsWith("rule ") then
            val ruleId = policyLine.substring("rule ".length).trim
            i += 1
            var ruleDescription = ""
            var matcherStr = ""
            var replacementStr = ""
            var priority = 100
            var enabled = true
            var inRule = true

            // тут читаем правила до end
            while inRule && i < lines.size do
              val ruleLine = lines(i)
              if ruleLine == "end" then
                inRule = false
                i += 1
              else if ruleLine.startsWith("description=") then
                ruleDescription = afterEquals(ruleLine)
                i += 1
              else if ruleLine.startsWith("matcher=") then
                matcherStr = afterEquals(ruleLine)
                i += 1
              else if ruleLine.startsWith("replacement=") then
                replacementStr = afterEquals(ruleLine)
                i += 1
              else if ruleLine.startsWith("priority=") then
                priority = afterEquals(ruleLine).toIntOption.getOrElse(100)
                i += 1
              else if ruleLine.startsWith("enabled=") then
                enabled = afterEquals(ruleLine) == "true"
                i += 1
              else
                i += 1

            // создаем обьект "Правило" и добавляем в список текущей рассматриваемой политики
            rules += Rule(
              id = ruleId,
              description = ruleDescription,
              matcher = parseMatcher(matcherStr),
              replacement = parseReplacement(replacementStr),
              priority = priority,
              enabled = enabled
            )
          else
            i += 1

        // политика прочитана, создаем обьект политика и добавляем в список политик
        policies += Policy(policyName, description, includes, rules.toSeq)
      else
        // если не политика и не правило, то ключ=значение
        val idx = line.indexOf('=')
        if idx >= 0 then
          val key = line.substring(0, idx).trim
          val value = line.substring(idx + 1).trim

          // присваиваем значение
          key match
            case "logFile" =>
              logFile = value
            case "reportDirectory" =>
              reportDirectory = value
            case "outputDirectory" =>
              outputDirectory = value
            case "defaultPolicy" =>
              defaultPolicy = value
            case "overwriteExistingFiles" =>
              overwriteExistingFiles = value == "true"
            case "supportedExtensions" =>
              supportedExtensions = value
                .split(",")
                .map(_.trim)
                .filter(_.nonEmpty)
                .toSeq
            case _ =>
              ()
        i += 1

    // собираем ВСЕ в итоговой обьект конфиг
    Config(
      AppSettings(
        logFile = logFile,
        reportDirectory = reportDirectory,
        outputDirectory = outputDirectory,
        defaultPolicy = defaultPolicy,
        overwriteExistingFiles = overwriteExistingFiles,
        supportedExtensions = supportedExtensions
      ),
      policies.toSeq
    )

  // сохранение в файл, превращаем обьект обратно в текст и записываем на диск, нужно для сохранения изменений
  def save(path: String, config: Config): Unit =
    val sb = new StringBuilder

    // запись глобальных настроек
    sb.append(s"logFile=${config.settings.logFile}\n")
    sb.append(s"reportDirectory=${config.settings.reportDirectory}\n")
    sb.append(s"outputDirectory=${config.settings.outputDirectory}\n")
    sb.append(s"defaultPolicy=${config.settings.defaultPolicy}\n")
    sb.append(s"overwriteExistingFiles=${config.settings.overwriteExistingFiles}\n")
    sb.append(s"supportedExtensions=${config.settings.supportedExtensions.mkString(",")}\n")

    // запись политик и всех правил
    config.policies.foreach { policy =>
      sb.append(s"\npolicy ${policy.name}\n")
      sb.append(s"description=${policy.description}\n")
      sb.append(s"includes=${policy.includes.mkString(",")}\n")

      policy.rules.foreach { rule =>
        sb.append(s"rule ${rule.id}\n")
        sb.append(s"description=${rule.description}\n")
        sb.append(s"matcher=${matcherToString(rule.matcher)}\n")
        sb.append(s"replacement=${replacementToString(rule.replacement)}\n")
        sb.append(s"priority=${rule.priority}\n")
        sb.append(s"enabled=${rule.enabled}\n")
        sb.append("end\n")
      }

      sb.append("end\n")
    }

    // запись строки в файл
    val p = Paths.get(path)

    if p.getParent != null then
      Files.createDirectories(p.getParent)

    Files.write(p, sb.toString.getBytes(StandardCharsets.UTF_8))

  // стандартный конфиг
    //если файла нет, то программа использует строгий набор политик
  def defaultConfig: Config =
    val personalData = Policy(
      name = "personal-data",
      description = "Персональные данные",
      includes = Seq.empty,
      rules = Seq(
        Rule(
          id = "emails",
          description = "Адреса электронной почты",
          matcher = BuiltinMatcher("email"),
          replacement = PseudonymReplacement("user{number}@example.com"),
          priority = 100,
          enabled = true
        ),
        Rule(
          id = "phones",
          description = "Телефонные номера",
          matcher = BuiltinMatcher("phone"),
          replacement = ConstantReplacement("[PHONE]"),
          priority = 90,
          enabled = true
        )
      )
    )

    val credentials = Policy(
      name = "credentials",
      description = "Пароли и токены",
      includes = Seq.empty,
      rules = Seq(
        Rule(
          id = "password-fields",
          description = "Пароли",
          matcher = BuiltinMatcher("password"),
          replacement = ConstantReplacement("[REDACTED]"),
          priority = 200,
          enabled = true
        ),
        Rule(
          id = "tokens",
          description = "Токены и ключи доступа",
          matcher = BuiltinMatcher("token"),
          replacement = ConstantReplacement("[REDACTED]"),
          priority = 200,
          enabled = true
        )
      )
    )

    val networkData = Policy(
      name = "network-data",
      description = "Сетевые данные",
      includes = Seq.empty,
      rules = Seq(
        Rule(
          id = "ipv4",
          description = "IPv4 адреса",
          matcher = BuiltinMatcher("ipv4"),
          replacement = PseudonymReplacement("host{number}"),
          priority = 80,
          enabled = true
        ),
        Rule(
          id = "url",
          description = "URL",
          matcher = BuiltinMatcher("url"),
          replacement = ConstantReplacement("[URL]"),
          priority = 70,
          enabled = true
        )
      )
    )

    val strict = Policy(
      name = "strict",
      description = "Полная очистка перед публикацией",
      includes = Seq("personal-data", "credentials", "network-data"),
      rules = Seq(
        Rule(
          id = "emails",
          description = "Адреса электронной почты",
          matcher = BuiltinMatcher("email"),
          replacement = ConstantReplacement("[EMAIL]"),
          priority = 150,
          enabled = true
        )
      )
    )

    val settings = AppSettings(
      logFile = "anonymizer.log",
      reportDirectory = "reports",
      outputDirectory = "anonymized",
      defaultPolicy = "strict",
      overwriteExistingFiles = false,
      supportedExtensions = Seq(".txt", ".log", ".md", ".csv", ".json", ".xml")
    )

    Config(settings, Seq(personalData, credentials, networkData, strict))




  // отрезаем все, что стоит после знака =
  private def afterEquals(line: String): String =
    val idx = line.indexOf('=')
    if idx < 0 then "" else line.substring(idx + 1).trim

  // превращает строку из конфига в BuiltinMatcher (поиск по встроенному типу)
  private def parseMatcher(value: String): Matcher =
    if value.startsWith("builtin:") then
      BuiltinMatcher(value.stripPrefix("builtin:").trim)
    else if value.startsWith("regex:") then
      RegexMatcher(value.stripPrefix("regex:").trim)
    else if value.startsWith("exact:") then
      ExactMatcher(value.stripPrefix("exact:").trim)
    else if value.startsWith("dict:") then
      DictionaryMatcher(
        value
          .stripPrefix("dict:")
          .split('|')
          .map(_.trim)
          .filter(_.nonEmpty)
          .toSeq
      )
    else
      ExactMatcher(value)

  // тоже самое но Replacement
  private def parseReplacement(value: String): Replacement =
    if value.startsWith("constant:") then
      ConstantReplacement(value.stripPrefix("constant:"))
    else if value == "delete" then
      DeleteReplacement
    else if value.startsWith("mask:") then
      val parts = value.stripPrefix("mask:").split(':')

      val start = parts.lift(0).flatMap(_.toIntOption).getOrElse(1)
      val end = parts.lift(1).flatMap(_.toIntOption).getOrElse(1)
      val ch = parts.lift(2).flatMap(_.headOption).getOrElse('*')

      MaskReplacement(start, end, ch)
    else if value.startsWith("pseudonym:") then
      PseudonymReplacement(value.stripPrefix("pseudonym:"))
    else if value.startsWith("list:") then
      ListReplacement(
        value
          .stripPrefix("list:")
          .split('|')
          .map(_.trim)
          .filter(_.nonEmpty)
          .toSeq
      )
    else
      ConstantReplacement(value)

  // превращает обьекты в строки для записи в файл
  private def matcherToString(matcher: Matcher): String =
    matcher match
      case BuiltinMatcher(kind) =>
        s"builtin:$kind"
      case RegexMatcher(pattern) =>
        s"regex:$pattern"
      case ExactMatcher(value) =>
        s"exact:$value"
      case DictionaryMatcher(values) =>
        s"dict:${values.mkString("|")}"

  private def replacementToString(replacement: Replacement): String =
    replacement match
      case ConstantReplacement(value) =>
        s"constant:$value"
      case DeleteReplacement =>
        "delete"
      case MaskReplacement(visibleStart, visibleEnd, maskChar) =>
        s"mask:$visibleStart:$visibleEnd:$maskChar"
      case PseudonymReplacement(template) =>
        s"pseudonym:$template"
      case ListReplacement(values) =>
        s"list:${values.mkString("|")}"