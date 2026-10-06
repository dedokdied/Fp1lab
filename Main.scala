import scala.io.StdIn

class AnonymizerApp:
  private val configPath = ConfigLoader.DefaultPath // путь к файлу конфига

  // пытаемся загрузить конфиг
  private var config: Config =
    try ConfigLoader.loadOrDefault(configPath)
    catch
      case e: Exception =>
        println(s"Ошибка загрузки конфигурации: ${e.getMessage}")
        println("Будет использована конфигурация по умолчанию.")
        ConfigLoader.defaultConfig

  // храним отчет о последней обработке
  private var lastReport: Option[OperationReport] = None

  // команда для записи события в лог файл
  private def log(message: String): Unit =
    Engine.log(config.settings.logFile, message)

  // сохраняем текущий конфиг в файл
  private def saveConfig(): Unit =
    try
      ConfigLoader.save(configPath, config)
      log("Конфигурация сохранена")
    catch
      case e: Exception =>
        println(s"Не удалось сохранить конфигурацию: ${e.getMessage}")

  def start(): Unit =
    // создаем главынй экран
    val menu = MenuContainer(
      "Менеджер политик анонимизации",
      Seq(
        MenuItem("Показать все политики", () => showPolicies()), //
        MenuItem("Показать правила политики", () => showPolicy()), //
        MenuItem("Проверить политику", () => validatePolicyAction()), //
        MenuItem("Создать пустую политику", () => createPolicy()), //
        MenuItem("Изменить политику", () => editPolicyMenu()), //
        MenuItem("Добавить встроенное правило", () => addBuiltinRule()), //
        MenuItem("Составить политику", () => composePolicy()),//
        MenuItem("Удалить политику", () => deletePolicy()),//
        MenuItem("Предпросмотр обработки", () => runAnonymize(writeFiles = false)),//
        MenuItem("Анонимизировать файл/каталог", () => runAnonymize(writeFiles = true)),//
        MenuItem("Показать последний отчёт", () => showReport()),//
        MenuItem("Перезагрузить конфигурацию", () => reloadConfig()),//
        MenuItem("Сохранить конфигурацию", () => saveConfigAction())//
      )
    )
    val controller = MenuController(menu)
    controller.menuLoop()

  // считывает строку, убирает лишние проблемы по краям
  private def ask(prompt: String): String =
    print(prompt)
    val line = StdIn.readLine()
    if line == null then "" else line.trim

  // считывает число, если пользователь ввел букву, то вернет значение default
  private def askInt(prompt: String, default: Int): Int =
    val value = ask(prompt)
    if value.isEmpty then default
    else value.toIntOption.getOrElse(default)

  // показывает список всех политик
  private def showPolicies(): Unit =
    if config.policies.isEmpty then
      println("Политик пока нет")
    else
      config.policies.foreach { policy =>
        val includesText =
          if policy.includes.isEmpty then ""
          else s" включает: ${policy.includes.mkString(", ")}"
        println(s"- ${policy.name}$includesText")
      }

  // ищет политику по имени
  private def showPolicy(): Unit =
    val name = ask("Имя политики: ")
    config.findPolicy(name) match // config
      case None =>
        println("Политика не найдена")
      case Some(policy) =>
        println(s"Политика: ${policy.name}")
        println(s"Описание: ${policy.description}")
        if policy.includes.nonEmpty then
          println(s"Включает: ${policy.includes.mkString(", ")}")
        if policy.rules.isEmpty then
          println("Правил нет")
        else
          policy.rules.foreach(printRule)

  // Красивый вывод правила
  private def printRule(rule: Rule): Unit =
    println(s"  - ${rule.id}: ${rule.description}")
    println(s"    поиск: ${matcherText(rule.matcher)}")
    println(s"    замена: ${replacementText(rule.replacement)}")
    println(s"    приоритет: ${rule.priority}, активно: ${rule.enabled}")

  // Методы просто превращают обьекты Поиска и замены в обычный текст для вывода на экран
  private def matcherText(matcher: Matcher): String =
    matcher match
      case BuiltinMatcher(kind) =>
        s"builtin:$kind"
      case RegexMatcher(pattern) =>
        s"regex:$pattern"
      case ExactMatcher(value) =>
        s"exact:$value"
      case DictionaryMatcher(values) =>
        s"dict:${values.mkString("|")}"
  private def replacementText(replacement: Replacement): String =
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

  // Запускает валидацию(проверку данных) на движке и выводит список ошибок, если они есть
  private def validatePolicyAction(): Unit =
    val name = ask("Имя политики: ")
    val errors = Engine.validatePolicy(name, config) // engine
    if errors.isEmpty then
      println("Политика корректна")
    else
      errors.foreach(error => println("- " + error))

  // создать политику без правил и добавить в конфиг
  private def createPolicy(): Unit =
    val name = ask("Имя новой политики: ")
    if name.isEmpty then
      println("Имя не может быть пустым")
    else if config.findPolicy(name).nonEmpty then // domain
      println("Такая политика уже существует")
    else
      val description = ask("Описание: ")
      config = config.withPolicy(Policy(name, description, Seq.empty, Seq.empty)) // domain
      saveConfig() // config loader
      println(s"Политика '$name' создана")
      log(s"Создана политика: $name")

  // открывает подменю для изменения политики
  private def editPolicyMenu(): Unit =
    val name = ask("Имя политики для изменения: ")
    config.findPolicy(name) match // domain
      case None =>
        println("Политика не найдена")
      case Some(policy) =>
        val sub = MenuContainer(
          s"Изменение политики ${policy.name}",
          Seq(
            MenuItem("Изменить описание", () => changePolicyDescription(name)),
            MenuItem("Задать список includes", () => changePolicyIncludes(name)),
            MenuItem("Изменить правило", () => editRuleMenu(name)),
            MenuItem("Удалить правило", () => deleteRuleFromPolicy(name))
          )
        )
        MenuController(sub).menuLoop()

  // изменение описания политики
  private def changePolicyDescription(policyName: String): Unit =
    config.findPolicy(policyName).foreach { p =>
      println(s"Текущее описание: ${p.description}")
      val d = ask("Новое описание: ")
      config = config.withPolicy(p.copy(description = d))
      saveConfig()
      println("Описание обновлено")
      log(s"Политика $policyName изменена: описание")
    }

  // меняет список включаемых в политику политик
  private def changePolicyIncludes(policyName: String): Unit =
    config.findPolicy(policyName).foreach { p =>
      val current = if p.includes.isEmpty then "(нет)" else p.includes.mkString(", ")
      println(s"Текущие includes: $current")
      val s = ask("Новые includes через запятую (пусто = очистить): ")
      val includes = s.split(",").map(_.trim).filter(_.nonEmpty).toSeq
      config = config.withPolicy(p.copy(includes = includes))
      saveConfig()
      val errors = Engine.validatePolicy(policyName, config)
      if errors.nonEmpty then
        println("Предупреждения после изменения:")
        errors.foreach(e => println("- " + e))
      else
        println("Includes обновлены")
      log(s"Политика $policyName изменена: includes")
    }

  // Открывает подменю для изменения конкретного правила, показывает список правил, а потом подменю для выбранного
  private def editRuleMenu(policyName: String): Unit =
    config.findPolicy(policyName) match
      case None =>
        println("Политика не найдена")
      case Some(policy) =>
        if policy.rules.isEmpty then
          println("В политике нет правил")
        else
          policy.rules.zipWithIndex.foreach { case (r, i) =>
            println(s"${i + 1}) ${r.id} — приоритет ${r.priority}, активно: ${r.enabled}")
          }
          val idx = askInt("Номер правила: ", 0) - 1
          if idx < 0 || idx >= policy.rules.size then
            println("Неверный номер")
          else
            val rule = policy.rules(idx)
            val sub = MenuContainer(
              s"Правило ${rule.id} (политика $policyName)",
              Seq(
                MenuItem("Изменить приоритет", () => changeRulePriority(policyName, rule.id)),
                MenuItem("Включить/выключить", () => toggleRuleEnabled(policyName, rule.id)),
                MenuItem("Изменить замену", () => changeRuleReplacement(policyName, rule.id)),
                MenuItem("Изменить описание правила", () => changeRuleDescription(policyName, rule.id))
              )
            )
            MenuController(sub).menuLoop()

  // Изменение правила, находит политику, правило, выбирает функцию change, сохраняет в конфиг и пишет в лог
  private def updateRule(policyName: String, ruleId: String)(change: Rule => Rule): Unit =
    config.findPolicy(policyName).foreach { p =>
      val newRules = p.rules.map(r => if r.id == ruleId then change(r) else r)
      config = config.withPolicy(p.copy(rules = newRules))
      saveConfig()
      log(s"Политика $policyName изменена: правило $ruleId")
    }

  // пример использовани изменения политики
  private def changeRulePriority(policyName: String, ruleId: String): Unit =
    val v = askInt("Новый приоритет: ", 100)
    updateRule(policyName, ruleId)(r => r.copy(priority = v))
    println(s"Правило $ruleId: приоритет теперь $v")


  private def toggleRuleEnabled(policyName: String, ruleId: String): Unit =
    config.findPolicy(policyName).foreach { p =>
      p.rules.find(_.id == ruleId).foreach { r =>
        val newState = !r.enabled
        updateRule(policyName, ruleId)(x => x.copy(enabled = newState))
        println(s"Правило $ruleId: активно = $newState")
      }
    }

  private def changeRuleReplacement(policyName: String, ruleId: String): Unit =
    val rep = readReplacement()
    updateRule(policyName, ruleId)(r => r.copy(replacement = rep))
    println(s"Правило $ruleId: замена изменена")

  private def changeRuleDescription(policyName: String, ruleId: String): Unit =
    val d = ask("Новое описание правила: ")
    updateRule(policyName, ruleId)(r => r.copy(description = d))
    println(s"Правило $ruleId: описание изменено")

  // удаляет правило из политики по ид, оставляем все правила кроме удаляемого
  private def deleteRuleFromPolicy(policyName: String): Unit =
    config.findPolicy(policyName).foreach { p =>
      if p.rules.isEmpty then
        println("В политике нет правил")
      else
        p.rules.foreach(r => println(s"- ${r.id}"))
        val id = ask("ID правила для удаления: ")
        if p.rules.exists(_.id == id) then
          config = config.withPolicy(p.copy(rules = p.rules.filterNot(_.id == id)))
          saveConfig()
          println(s"Правило $id удалено")
          log(s"Политика $policyName изменена: правило $id удалено")
        else
          println("Правила с таким ID в этой политике нет")
    }

  // добавляет новое правило с встроенным типом поиска в выбранную политику
  private def addBuiltinRule(): Unit =
    val policyName = ask("Политика, в которую добавить правило: ")
    config.findPolicy(policyName) match
      case None =>
        println("Политика не найдена")
      case Some(policy) =>
        val ruleId = ask("ID правила: ")
        if ruleId.isEmpty then
          println("ID не может быть пустым")
        else if policy.rules.exists(_.id == ruleId) then
          println("Правило с таким ID уже есть в политике")
        else
          val kind = ask("Встроенный тип (email/phone/ipv4/url/password/token): ")
          val replacement = readReplacement()
          val priority = askInt("Приоритет [100]: ", 100)
          val rule = Rule(
            id = ruleId,
            description = s"Правило $ruleId",
            matcher = BuiltinMatcher(kind),
            replacement = replacement,
            priority = priority,
            enabled = true
          )
          config = config.withPolicy(policy.copy(rules = policy.rules :+ rule))
          saveConfig()
          println(s"Правило '$ruleId' добавлено в политику '$policyName'")
          log(s"Добавлено правило $ruleId в политику $policyName")

  // сохздание нужного обьекта замены в зависимости от выбора пользователя
  private def readReplacement(): Replacement =
    val replacementType = ask("Тип замены (constant/delete/mask/pseudonym/list): ").toLowerCase
    replacementType match
      case "constant" =>
        ConstantReplacement(ask("Строка замены: "))
      case "delete" =>
        DeleteReplacement
      case "mask" =>
        val start = askInt("Сколько символов оставить в начале [2]: ", 2)
        val end = askInt("Сколько символов оставить в конце [2]: ", 2)
        val ch = ask("Символ маски [*]: ")
        MaskReplacement(start, end, ch.headOption.getOrElse('*'))
      case "pseudonym" =>
        PseudonymReplacement(ask("Шаблон с {number}: "))
      case "list" =>
        ListReplacement(
          ask("Значения через |: ")
            .split('|')
            .map(_.trim)
            .filter(_.nonEmpty)
            .toSeq
        )
      case _ =>
        ConstantReplacement("[REDACTED]")

  // Создание составной политики
  private def composePolicy(): Unit =
    val name = ask("Имя составной политики: ")
    if name.isEmpty then
      println("Имя не может быть пустым")
    else
      val description = ask("Описание: ")
      val includes = ask("Включаемые политики через запятую: ")
        .split(",")
        .map(_.trim)
        .filter(_.nonEmpty)
        .toSeq
      config = config.withPolicy(Policy(name, description, includes, Seq.empty))
      saveConfig()
      println(s"Политика '$name' создана")
      val errors = Engine.validatePolicy(name, config)
      if errors.nonEmpty then
        println("Предупреждения/ошибки:")
        errors.foreach(error => println("- " + error))
      log(s"Составлена политика: $name")

  // удаление политики по имени
  private def deletePolicy(): Unit =
    val name = ask("Имя политики для удаления: ")
    if config.findPolicy(name).isEmpty then
      println("Политика не найдена")
    else
      config = config.withoutPolicy(name)
      saveConfig()
      println(s"Политика '$name' удалена")
      log(s"Удалена политика: $name")

  // главный метод обработки, если writeFiles = false, то предпросмотр и тд
  private def runAnonymize(writeFiles: Boolean): Unit =
    val policyName = ask("Политика: ")
    val path = ask("Файл или каталог: ")
    //вызываем двигатель Either возвращает ошибку или отчет
    Engine.anonymize(policyName, path, config, writeFiles) match
      case Left(error) =>
        println(s"Ошибка: $error")
        Engine.log(config.settings.logFile, s"Ошибка команды: $error")
      case Right(report) =>
        lastReport = Some(report)
        println(report.summary)
        if !writeFiles then {
          // предпросмотр, показываем детали замнг
          report.fileResults.take(1).foreach(showFilePreview)
        } else
          // реальная обработка
          val problems = report.fileResults.filter(r => r.error.nonEmpty || r.skipped)
          problems.take(10).foreach { fileResult =>
            val status =
              if fileResult.error.nonEmpty then fileResult.error.getOrElse("Ошибка")
              else "Пропущен"
            println(s"Файл ${fileResult.relativePath}: $status")
          }

  // предпросмотр замен для одного файла
  private def showFilePreview(fileResult: FileResult): Unit =
    println(s"Файл: ${fileResult.relativePath}")
    fileResult.preview match
      case None =>
        println("  Нет данных для предпросмотра")
      case Some(process) =>
        if process.applied.isEmpty then
          println("  Совпадений не найдено")
        else
          process.applied.take(10).foreach { applied =>
            println(s"  ${applied.original} -> ${applied.replacementText}")
          }
          if process.applied.size > 10 then
            println(s"  ... ещё ${process.applied.size - 10}")
        process.conflicts.take(5).foreach { conflict =>
          println(s"  Конфликт: ${conflict.ruleIds.mkString(", ")}")
        }

  // показывает сохраненный ранее отчет
  private def showReport(): Unit =
    lastReport match
      case None =>
        println("Отчётов ещё нет")
      case Some(report) =>
        println(report.summary)
        report.fileResults.filter(_.error.nonEmpty).foreach { fileResult =>
          println(s"Ошибка файла: ${fileResult.relativePath}: ${fileResult.error.getOrElse("")}")
        }
        report.fileResults.filter(_.skipped).foreach { fileResult =>
          println(s"Файл пропущен: ${fileResult.relativePath}")
        }

  // перезагружаем конфиг из файла
  private def reloadConfig(): Unit =
    try
      config = ConfigLoader.load(configPath)
      println("Конфигурация перезагружена")
      log("Конфигурация перезагружена")
    catch
      case e: Exception =>
        println(s"Не удалось перезагрузить конфигурацию: ${e.getMessage}")
        println("Продолжаю использовать последнюю корректную версию.")

  // ручное сохранение конфига
  private def saveConfigAction(): Unit =
    saveConfig()
    println("Конфигурация сохранена")

// вход в программу
@main
def run(): Unit =
  val app = new AnonymizerApp
  app.start()