import scala.annotation.tailrec

object Assignment1 {
  def main(args: Array[String]): Unit = {
    val weekdays: List[String] = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    val ints: List[Int] = List(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

    println("Task 1")
    print("a. ")
    println(getWeekdays(weekdays))
    print("b. ")
    println(getSDays(weekdays))
    print("c. ")
    println(getWeekdaysViaWhile(weekdays))
    println("\nTask 2")
    print("a. ")
    println(getWeekdaysRecursion(weekdays))
    print("b. ")
    println(getWeekdaysRecursionRev(weekdays))
    println("\nTask 3")
    println(getWeekdaysTailRec(weekdays))
    println("\nTask 6")
    println(getIncrementedInts(ints))
  }

  def getWeekdays(weekdays: List[String]): String = {
    var str: String = ""
    for (weekday <- weekdays) {
      if (!weekday.equals(weekdays.last)) {
        str = str + weekday + ", "
      } else {
        str = str + weekday
      }
    }
    str
  }

  def getSDays(weekdays: List[String]): String = {
    var str: String = ""
    for (weekday <- weekdays if weekday.startsWith("S")) {
      if (!weekday.equals(weekdays.last)) {
        str = str + weekday + ", "
      } else {
        str = str + weekday
      }
    }
    str
  }

  def getWeekdaysViaWhile(weekdays: List[String]): String = {
    var str: String = ""
    var index = 0
    while (index < weekdays.length) {
      if (!weekdays(index).equals(weekdays.last)) {
        str = str + weekdays(index) + ", "
        index = index + 1
      } else {
        str = str + weekdays(index)
        index = index + 1
      }
    }
    str
  }

  def getWeekdaysRecursion(weekdays: List[String]): String = {
    if (weekdays.isEmpty)
      ""
    else if (!weekdays.head.equals(weekdays.last)) {
      weekdays.head + ", " + getWeekdaysRecursion(weekdays.tail)
    } else weekdays.head + getWeekdaysRecursion(weekdays.tail)
  }

  def getWeekdaysRecursionRev(weekdays: List[String]): String = {
    if (weekdays.isEmpty)
      ""
    else weekdays.last + ", " + getWeekdaysRecursionRev(weekdays.reverse.tail)
  }

  def getWeekdaysTailRec(weekdays: List[String]): String = {
    @tailrec def tailRec(acc: String, list: List[String]): String = {
      if (list.isEmpty)
        acc
      else if (!list.head.equals(list.last)) {
        tailRec(acc + list.head + ", ", list.tail)
      } else tailRec(acc + list.head, list.tail)
    }
    tailRec("", weekdays)
  }

  def getIncrementedInts(ints: List[Int]): List[Int] = {
      ints.map(n => n+1)
  }
}