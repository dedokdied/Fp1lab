import java.nio.file.*
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import scala.util.Try

// раскрытие составной политики (политика, которая состоит из нескольких политик), список правил и предупреждений
case class ExpandedPolicy(
                           name: String,
                           rules: Seq[Rule],
                           warnings: Seq[String]
                         )

// найденное совпадение: координаты в тексте + правило + оригинальный текст
case class FoundMatch(
                       ruleId: String,
                       priority: Int,
                       start: Int,
                       end: Int,
                       original: String,
                       replacement: Replacement
                     )

// примененная замена
case class AppliedMatch(
                         ruleId: String,
                         start: Int,
                         end: Int,
                         original: String,
                         replacementText: String
                       )

// конфликт двух правил
case class Conflict(
                     message: String,
                     start: Int,
                     end: Int,
                     ruleIds: Seq[String]
                   )

// результат обработки текста
case class ProcessResult(
                          outputText: String,
                          applied: Seq[AppliedMatch], // замены
                          conflicts: Seq[Conflict], // конфликты
                          countsByRule: Map[String, Int] // статистика
                        )

// результат обработки одного файла
case class FileResult(
                       relativePath: String,
                       sourcePath: String,
                       outputPath: String,
                       replaced: Int,
                       conflicts: Int,
                       error: Option[String],
                       skipped: Boolean,
                       preview: Option[ProcessResult]
                     )

// итоговый отчет по всей операции
case class OperationReport(
                            policyName: String,
                            startedAt: String,
                            finishedAt: String,
                            source: String,
                            output: String,
                            filesFound: Int,
                            filesProcessed: Int,
                            filesSkipped: Int,
                            filesFailed: Int,
                            replacedTotal: Int,
                            conflictsTotal: Int,
                            fileResults: Seq[FileResult]
                          ):
  def summary: String =
    s"""Обработка завершена.
       |Политика: $policyName
       |Начало: $startedAt
       |Завершение: $finishedAt
       |Источник: $source
       |Результат: $output
       |Файлов найдено: $filesFound
       |Обработано файлов: $filesProcessed
       |Пропущено файлов: $filesSkipped
       |Ошибок: $filesFailed
       |Создано замен: $replacedTotal
       |Конфликтов: $conflictsTotal
       |""".stripMargin

// хранит связь для будущей замены, "оригинальное значение - номер/значение" для дубликатов, чтобы одни и те же данные помечались одинаково
class ReplacementState:
  private var nextNumber = 1
  private var numbers = Map.empty[String, Int]
  private var listAssignments = Map.empty[String, String]
  private var listCounter = 0

  // получить значение для псевдонима, если значение уже встречалось - возвращаем тот же номер
  def numberFor(original: String): Int =
    numbers.get(original) match
      case Some(number) =>
        number
      case None =>
        val number = nextNumber
        nextNumber += 1
        numbers = numbers.updated(original, number)
        number

  // получить значение из списка для замены, Если значение уже встречалось, вернем то же
  def listValueFor(original: String, values: Seq[String]): String =
    listAssignments.get(original) match
      case Some(value) =>
        value
      case None =>
        val value = values(listCounter % values.size)
        listCounter += 1
        listAssignments = listAssignments.updated(original, value)
        value

// набор типов поиска, чтобы пользователь не вводил не существующий тип
object Engine:
  private val supportedBuiltinKinds = Set("email", "phone", "ipv4", "url", "password", "token")

  // текущее время для логов и отчетов
  private def now(): String =
    LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))

  // запись сообщения в журнал, в лог файл если точнее
  def log(logFile: String, message: String): Unit =
    Try {
      val line = s"[${now()}] $message\n"
      val path = Paths.get(logFile)

      if path.getParent != null then
        Files.createDirectories(path.getParent)

      Files.write(
        path,
        line.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )
    }

  // метод раскрытия политик,
  def expandPolicy(name: String, config: Config): Either[String, ExpandedPolicy] =
    expand(name, config, Set.empty)

  // раскрывает политику собирая правила из всех включенных политик
  private def expand(
                      name: String,
                      config: Config,
                      visited: Set[String]
                    ): Either[String, ExpandedPolicy] =
    if visited.contains(name) then
      Left(s"Обнаружена циклическая зависимость в политике: $name")
    else
      config.findPolicy(name) match
        case None =>
          Left(s"Политика не найдена: $name")
        case Some(policy) =>
          val nextVisited = visited + name
          val includeResults = policy.includes.map(include => expand(include, config, nextVisited))
          val firstError = includeResults.collectFirst { case Left(error) => error }

          firstError match
            case Some(error) =>
              Left(error)
            case None =>
              val expandedIncludes = includeResults.collect { case Right(value) => value }
              val includedRules = expandedIncludes.flatMap(_.rules)
              val includedWarnings = expandedIncludes.flatMap(_.warnings)
              val allRules = includedRules ++ policy.rules
              val (finalRules, overrideWarnings) = overrideRules(allRules)

              Right(ExpandedPolicy(name, finalRules, includedWarnings ++ overrideWarnings))

  // переопределение правил, побеждает послежнее правило с данным id
  private def overrideRules(rules: Seq[Rule]): (Seq[Rule], Seq[String]) =
    var result = Vector.empty[Rule]
    var warnings = Vector.empty[String]

    rules.foreach { rule =>
      if result.exists(_.id == rule.id) then
        warnings :+= s"Правило '${rule.id}' было переопределено"

      result = result.filterNot(_.id == rule.id) :+ rule
    }

    (result, warnings)

  // Валидация конкретной политики
  def validatePolicy(name: String, config: Config): Seq[String] =
    val errors = ArrayBuffer[String]()

    // проверка на существование
    if config.findPolicy(name).isEmpty then
      errors += s"Политика не найдена: $name"

    // проверка всех политик в конфиге на дубликаты id внутри одной политики
    config.policies.foreach { policy =>
      val duplicateIds = policy.rules
        .groupBy(_.id)
        .collect { case (id, rules) if rules.size > 1 => id }
        .toSeq

      duplicateIds.foreach { id =>
        errors += s"В политике '${policy.name}' повторяется идентификатор правила: $id"
      }

      policy.includes.foreach { include =>
        if config.findPolicy(include).isEmpty then
          errors += s"Политика '${policy.name}' включает несуществующую политику: $include"
      }

      // валидация каждого правила
      policy.rules.foreach { rule =>
        validateRule(rule).foreach { message =>
          errors += s"Политика '${policy.name}', правило '${rule.id}': $message"
        }
      }
    }

    // раскрытие политики, берет циклы и зависимости
    expandPolicy(name, config) match
      case Left(error) =>
        errors += error
      case Right(_) =>
        ()

    errors.distinct.toSeq

  // валидация отдельного правила, проверка корретности поиска и замены
  private def validateRule(rule: Rule): Seq[String] =
    val errors = ArrayBuffer[String]()

    if rule.id.trim.isEmpty then
      errors += "пустой идентификатор правила"

    rule.matcher match
      case BuiltinMatcher(kind) if !supportedBuiltinKinds.contains(kind.toLowerCase) =>
        errors += s"неизвестный встроенный тип: $kind"
      case RegexMatcher(pattern) if pattern.isEmpty || !isValidRegex(pattern) =>
        errors += "некорректное регулярное выражение"
      case ExactMatcher(value) if value.isEmpty =>
        errors += "пустое точное значение"
      case DictionaryMatcher(values) if values.isEmpty =>
        errors += "пустой словарь"
      case _ =>
        ()

    rule.replacement match
      case MaskReplacement(visibleStart, visibleEnd, _) if visibleStart < 0 || visibleEnd < 0 =>
        errors += "параметры маски не могут быть отрицательными"
      case PseudonymReplacement(template) if !template.contains("{number}") =>
        errors += "в шаблоне псевдонима отсутствует {number}"
      case ListReplacement(values) if values.isEmpty =>
        errors += "пустой список замены"
      case _ =>
        ()

    errors.toSeq

  private def isValidRegex(pattern: String): Boolean =
    Try(Pattern.compile(pattern)).isSuccess

  // обработка текста - найти все совпадения, развешить конфликты и применить замены
  def processText(text: String, rules: Seq[Rule]): ProcessResult =
    processText(text, rules, new ReplacementState)

  def processText(text: String, rules: Seq[Rule], state: ReplacementState): ProcessResult =
    // найти все совпадения по правилам
    val allMatches = rules.filter(_.enabled).flatMap(findMatches(_, text))
    // выбрать победителей среди пересекающихся совпадений
    val (selected, conflicts) = resolveMatches(allMatches)

    // для каждого победителя определяем замену
    val withReplacements = selected.map { m =>
      (m, makeReplacement(m.original, m.replacement, state))
    }

    // создаем изменяемую копию текста
    val sb = new StringBuilder(text)

    // применяем замены с конца к началу
    withReplacements
      .sortBy { case (m, _) => -m.start }
      .foreach { case (m, replacementText) =>
        sb.replace(m.start, m.end, replacementText)
      }

    // формируем список примененных замен
    val applied = withReplacements
      .map { case (m, replacementText) =>
        AppliedMatch(m.ruleId, m.start, m.end, m.original, replacementText)
      }
      .sortBy(_.start)

    // сколько щзамен сделала каждое правило
    val counts = applied.groupBy(_.ruleId).map { case (ruleId, items) =>
      ruleId -> items.size
    }

    ProcessResult(sb.toString, applied, conflicts, counts)

  // раксрывает политику, обходит файлы и обрабатывает кажлый
  def anonymize(
                 policyName: String,
                 sourcePath: String,
                 config: Config,
                 writeFiles: Boolean
               ): Either[String, OperationReport] =
    val started = now()

    expandPolicy(policyName, config) match
      case Left(error) =>
        log(config.settings.logFile, s"Ошибка раскрытия политики: $error")
        Left(error)

      case Right(expanded) =>
        val source = Paths.get(sourcePath).toAbsolutePath.normalize

        if !Files.exists(source) then
          Left(s"Источник не найден: $source")
        else
          val outputRoot = Paths.get(config.settings.outputDirectory).toAbsolutePath.normalize

          if Files.isDirectory(source) && outputRoot.startsWith(source) then
            Left("Выходной каталог находится внутри обрабатываемого каталога")
          else
            val allFiles = collectAllFiles(source)
            val supportedFiles = allFiles.filter(isSupported(_, config.settings.supportedExtensions))

            log(
              config.settings.logFile,
              s"Начата обработка: политика=$policyName, источник=$source, файлов найдено=${allFiles.size}"
            )

            val replacementState = new ReplacementState

            val fileResults = supportedFiles.map { file =>
              processFile(
                file,
                source,
                outputRoot,
                expanded.rules,
                config.settings,
                writeFiles,
                replacementState
              )
            }

            val finished = now()

            val filesFound = allFiles.size
            val filesSkippedByExtension = filesFound - supportedFiles.size
            val filesSkippedExisting = fileResults.count(_.skipped)
            val filesProcessed = fileResults.count(r => r.error.isEmpty && !r.skipped)
            val filesFailed = fileResults.count(_.error.nonEmpty)

            val replacedTotal = fileResults
              .filter(r => r.error.isEmpty && !r.skipped)
              .map(_.replaced)
              .sum

            val conflictsTotal = fileResults.map(_.conflicts).sum

            val report = OperationReport(
              policyName = policyName,
              startedAt = started,
              finishedAt = finished,
              source = source.toString,
              output = if writeFiles then outputRoot.toString else "(предпросмотр)",
              filesFound = filesFound,
              filesProcessed = filesProcessed,
              filesSkipped = filesSkippedByExtension + filesSkippedExisting,
              filesFailed = filesFailed,
              replacedTotal = replacedTotal,
              conflictsTotal = conflictsTotal,
              fileResults = fileResults
            )

            log(
              config.settings.logFile,
              s"Обработка завершена: политика=$policyName, обработано=${report.filesProcessed}, пропущено=${report.filesSkipped}, ошибок=${report.filesFailed}"
            )

            Right(report)

  // обход файлов, если это файл, то возвращаем его, если каталог - обходим рекурсивно, применяется для получения списка файлов
  private def collectAllFiles(source: Path): Seq[Path] =
    if Files.isRegularFile(source) then
      Seq(source)
    else if Files.isDirectory(source) then
      val stream = Files.walk(source)
      try
        stream.iterator.asScala.filter(p => Files.isRegularFile(p)).toSeq
      finally
        stream.close()
    else
      Seq.empty


  //проверка, что расширение файлов разрешено
  private def isSupported(path: Path, extensions: Seq[String]): Boolean =
    if extensions.isEmpty then
      true
    else
      val name = path.getFileName.toString.toLowerCase
      extensions.exists(ext => name.endsWith(ext.toLowerCase))

  //обработка одного файла, читает файл, обрабатывает текст, записывает результат
  private def processFile(
                           file: Path,
                           source: Path,
                           outputRoot: Path,
                           rules: Seq[Rule],
                           settings: AppSettings,
                           writeFiles: Boolean,
                           replacementState: ReplacementState
                         ): FileResult =
    val relative =
      if Files.isDirectory(source) then source.relativize(file).toString
      else file.getFileName.toString

    val outPath = outputRoot.resolve(relative)

    try
      val text = new String(Files.readAllBytes(file), StandardCharsets.UTF_8)
      val result = processText(text, rules, replacementState)

      if writeFiles then
        if Files.exists(outPath) && !settings.overwriteExistingFiles then
          FileResult(
            relativePath = relative,
            sourcePath = file.toString,
            outputPath = outPath.toString,
            replaced = result.applied.size,
            conflicts = result.conflicts.size,
            error = None,
            skipped = true,
            preview = Some(result)
          )
        else
          if outPath.getParent != null then
            Files.createDirectories(outPath.getParent)

          Files.write(outPath, result.outputText.getBytes(StandardCharsets.UTF_8))

          FileResult(
            relativePath = relative,
            sourcePath = file.toString,
            outputPath = outPath.toString,
            replaced = result.applied.size,
            conflicts = result.conflicts.size,
            error = None,
            skipped = false,
            preview = Some(result)
          )
      else
        FileResult(
          relativePath = relative,
          sourcePath = file.toString,
          outputPath = outPath.toString,
          replaced = result.applied.size,
          conflicts = result.conflicts.size,
          error = None,
          skipped = false,
          preview = Some(result)
        )

    catch
      case e: Exception =>
        FileResult(
          relativePath = relative,
          sourcePath = file.toString,
          outputPath = outPath.toString,
          replaced = 0,
          conflicts = 0,
          error = Some(Option(e.getMessage).getOrElse("Неизвестная ошибка")),
          skipped = false,
          preview = None
        )

  // поиск совпадений, смотрит на тип правила и решает какую именно функцию вызвать для поиска текста
  private def findMatches(rule: Rule, text: String): Seq[FoundMatch] =
    if !rule.enabled then
      Seq.empty
    else
      rule.matcher match
        case BuiltinMatcher(kind) =>
          builtinPattern(kind) match
            case Some((pattern, group)) =>
              findPattern(pattern, group, text, rule)
            case None =>
              Seq.empty

        case RegexMatcher(pattern) =>
          if isValidRegex(pattern) then
            findPattern(Pattern.compile(pattern), 0, text, rule)
          else
            Seq.empty

        case ExactMatcher(value) =>
          findExactValue(value, text, rule)

        case DictionaryMatcher(values) =>
          values.flatMap(findExactValue(_, text, rule)).toSeq

  // паттерны, которые мы рассматриваем для замены
  private def builtinPattern(kind: String): Option[(Pattern, Int)] =
    kind.toLowerCase match
      case "email" =>
        Some((Pattern.compile("\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"), 0))

      case "phone" =>
        Some((Pattern.compile("(?:\\+7|8)?[\\s(]*\\d{3}[\\s)]*\\d{3}[\\s-]*\\d{2}[\\s-]*\\d{2}"), 0))

      case "ipv4" =>
        Some((Pattern.compile("\\b(?:\\d{1,3}\\.){3}\\d{1,3}\\b"), 0))

      case "url" =>
        Some((Pattern.compile("\\bhttps?://[^\\s<>\"']+"), 0))

      case "password" =>
        Some((Pattern.compile("(?i)(?:password|passwd|pwd)\\s*[=:]\\s*([^\\s;,\"]+)"), 1))

      case "token" =>
        Some(
          (
            Pattern.compile(
              "(?i)(?:token|api[_-]?key|secret|access[_-]?key|authorization)\\s*[=:]\\s*([^\\s;,\"]+)"
            ),
            1
          )
        )

      case _ =>
        None

  // делает поиск всех регулярных выражений в тексте
  private def findPattern(
                           pattern: Pattern,
                           group: Int,
                           text: String,
                           rule: Rule
                         ): Seq[FoundMatch] =
    val matcher = pattern.matcher(text)
    val buf = ArrayBuffer[FoundMatch]()

    while matcher.find() do
      val useGroup = group > 0 && matcher.groupCount() >= group && matcher.group(group) != null

      val start = if useGroup then matcher.start(group) else matcher.start()
      val end = if useGroup then matcher.end(group) else matcher.end()
      val original = if useGroup then matcher.group(group) else matcher.group()

      if original != null && original.nonEmpty then
        buf += FoundMatch(rule.id, rule.priority, start, end, original, rule.replacement)

    buf.toSeq

  // поиск точного значения, прохордит по тексту, находит все совпадения заданного слова, запоминает их координаты и вовращает список координат
  private def findExactValue(value: String, text: String, rule: Rule): Seq[FoundMatch] =
    if value.isEmpty then
      Seq.empty
    else
      val buf = ArrayBuffer[FoundMatch]()
      var index = text.indexOf(value)

      while index >= 0 do
        buf += FoundMatch(rule.id, rule.priority, index, index + value.length, value, rule.replacement)
        index = text.indexOf(value, index + value.length)

      buf.toSeq

  // разрешение конфликтов
  private def resolveMatches(matches: Seq[FoundMatch]): (Seq[FoundMatch], Seq[Conflict]) =
    val unique = matches.distinct

    if unique.isEmpty then
      (Seq.empty, Seq.empty)
    else
      val sorted = unique.sortBy(m => (m.start, -m.priority, -(m.end - m.start)))

      val clusters = ArrayBuffer[ArrayBuffer[FoundMatch]]()

      var current = ArrayBuffer(sorted.head)
      var currentEnd = sorted.head.end

      for m <- sorted.tail do
        if m.start <= currentEnd then
          current += m
          if m.end > currentEnd then currentEnd = m.end
        else
          clusters += current
          current = ArrayBuffer(m)
          currentEnd = m.end

      clusters += current

      val selected = ArrayBuffer[FoundMatch]()
      val conflicts = ArrayBuffer[Conflict]()

      clusters.foreach { cluster =>
        val maxPriority = cluster.map(_.priority).max
        val byPriority = cluster.filter(_.priority == maxPriority)
        val maxLength = byPriority.map(m => m.end - m.start).max
        val best = byPriority.filter(m => m.end - m.start == maxLength)

        if best.size == 1 then
          val winner = best.head
          selected += winner

          val losers = cluster.filterNot(_ == winner).map(_.ruleId).distinct.toSeq

          if losers.nonEmpty then
            conflicts += Conflict(
              message = "Найдено пересечение правил",
              start = winner.start,
              end = winner.end,
              ruleIds = (winner.ruleId +: losers).distinct
            )
        else
          conflicts += Conflict(
            message = "Невозможно выбрать правило",
            start = best.head.start,
            end = best.head.end,
            ruleIds = best.map(_.ruleId).distinct.toSeq
          )
      }

      (selected.toSeq, conflicts.toSeq)

  // генерация текста замены
  private def makeReplacement(
                               original: String,
                               replacement: Replacement,
                               state: ReplacementState
                             ): String =
    replacement match
      case ConstantReplacement(value) => // возвращает строку
        value

      case DeleteReplacement => // удаляет строку
        ""

      case MaskReplacement(visibleStart, visibleEnd, maskChar) => // макскирует внутри
        mask(original, visibleStart, visibleEnd, maskChar)

      case PseudonymReplacement(template) => // задает псевдоним
        val number = state.numberFor(original)
        template.replace("{number}", number.toString)

      case ListReplacement(values) => // заменяет значением из списка
        if values.isEmpty then "[REDACTED]"
        else state.listValueFor(original, values)

  // частичное максирование
  private def mask(original: String, visibleStart: Int, visibleEnd: Int, maskChar: Char): String =
    if original.isEmpty then
      original
    else
      val start = math.max(0, math.min(visibleStart, original.length))
      val end = math.max(0, math.min(visibleEnd, original.length - start))
      val hidden = original.length - start - end

      if hidden <= 0 then
        original
      else
        original.take(start) + maskChar.toString * hidden + original.takeRight(end)