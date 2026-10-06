import scala.io.StdIn

// в моем коде меню это по сути своей дерево

trait MenuTree:
  def name: String

class MenuItem(val name: String, val action: () => Unit) extends MenuTree // класс, который по сути описывает и активирует листы дерева (меню), например
// если у нас меню при запуске есть выбор от 1 до 13 + выход, то выбор одного из этих пунктов - это как раз вызов данной функции, т.е. активайция листа

class MenuContainer(val name: String, val content: Seq[MenuTree]) extends MenuTree // это контейнер, который внутри себя и содержит пункты меню

class MenuController(root: MenuContainer): // стек открытых экранов меню, 1 экран меню это например 1 экран (все 13 пунктов меню), мы выбираем один из пунктов
  // и этот пункт меню станет вторым экраном
  private var stack: List[MenuContainer] = List(root) // изначально только главное меню, остальные можно открыть, но по умолчанию они "закрыты"

  private def current: MenuContainer = stack.head

  private def printMenu(): Unit =
    println()
    println(s"=== ${current.name} ===") // заголовок текущего экрана
    current.content.zipWithIndex.foreach { case (item, i) =>
      println(s"${i + 1}) ${item.name}") 
    }

    if stack.size == 1 then // если мы в главном меню, то кнопка выхол, если нет, то назад
      println("0) Выход")
    else
      println("0) Назад")

  private def readChoice(): Int = // считываем выбор пользователя
    print("> ")
    val line = StdIn.readLine()
    // если значение неверное (не число или не то, что мы ожидаем увидеть, то просто даем ошибку и программа не падает)
    if line == null then 0
    else line.trim.toIntOption.getOrElse(-1)

  // главный цикл в файле меню
  def menuLoop(): Unit =
    var continue = true

    while continue do
      printMenu() // отрисовываем текущий экран
      val choice = readChoice() 

      if choice == 0 then
        if stack.size == 1 then
          continue = false // выход из программы
        else
          stack = stack.tail // убираем верхний экран

        // если ввели верный номер пункта, то создаем подменю (второй экран) и добавляем его в начало стека
      else if choice > 0 && choice <= current.content.size then
        current.content(choice - 1) match
          case container: MenuContainer =>
            stack = container :: stack // добавление тут
          case item: MenuItem => // остаемся на этом же экране, если это конечное действие
            item.action()
      else
        println("Неизвестный пункт. Введите число из меню.") // ну и отработка ошибок