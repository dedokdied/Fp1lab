// файл с описаниями всех структур данных

sealed trait Matcher // поиск

case class BuiltinMatcher(kind: String) extends Matcher // поиск по встроенному типу (например email и т.д.)
case class RegexMatcher(pattern: String) extends Matcher //поиск по выражению, которое задал пользователь
case class ExactMatcher(value: String) extends Matcher // поиск по точной строке найдет все упоминания по поиску в тексте
case class DictionaryMatcher(values: Seq[String]) extends Matcher // поиск по списку слов, найдет все слова из списка в тексте

sealed trait Replacement // замена

case class ConstantReplacement(value: String) extends Replacement // замена на фиксированную строку
case object DeleteReplacement extends Replacement // удаление найденного фрагмента для замены
case class MaskReplacement(visibleStart: Int, visibleEnd: Int, maskChar: Char) extends Replacement // маска для скрытия части значения
case class PseudonymReplacement(template: String) extends Replacement // замена на псевдоним с нумерацией (шаблон)
case class ListReplacement(values: Seq[String]) extends Replacement // замена на значения из списка (по очереди)

case class Rule(  // правило, если точнее, то что искать и как заменять
                 id: String,
                 description: String, // описание
                 matcher: Matcher,
                 replacement: Replacement, 
                 priority: Int, 
                 enabled: Boolean 
               )

case class Policy( // политика - набор правил или сборочная политика в виде нескольких политик сразу
                   name: String, 
                   description: String, 
                   includes: Seq[String], // список имен других политик, может быть пустым
                   rules: Seq[Rule] 
                 )

case class AppSettings( // глобольные настройки программы
                        logFile: String,
                        reportDirectory: String,
                        outputDirectory: String, // папка для анонимных копий
                        defaultPolicy: String,
                        overwriteExistingFiles: Boolean, // перезаписывался ли файл
                        supportedExtensions: Seq[String]
                      )

case class Config(settings: AppSettings, policies: Seq[Policy]): // класс, который хранит настройки и все политики
  def findPolicy(name: String): Option[Policy] = // найти политику по имени, если не найдена, то none
    policies.find(_.name == name)

  def withPolicy(policy: Policy): Config = // добавить политику или заменить существующую с таким же именем
    if policies.exists(_.name == policy.name) then
      copy(policies = policies.map(p => if p.name == policy.name then policy else p)) //замена
    else
      copy(policies = policies :+ policy) // добавляем новую в конец

  def withoutPolicy(name: String): Config = // удалить политику по имени
    copy(policies = policies.filterNot(_.name == name))
    copy(policies = policies.filterNot(_.name == name))