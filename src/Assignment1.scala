import scala.annotation.tailrec

object Assignment1 {
  def main(args: Array[String]): Unit = {
    val weekdays: List[String] = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    val ints: List[Int] = List(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    val goods = Map("rock" -> 15.0, "paper" -> 10.0, "scissors" -> 20.0, "wine" -> 45.0, "cheese" -> 58.0)
    val naturalNumbers: List[Int] = List(-10, -8, -5, 3, -4, 0, 2, -9, 10, 12, -11, 25, 30)
    val tuple = (1, "hello", 21.68)
    val intsZeroes: List[Int] = List(10, 0, 30, 40, 0, 60, 70, 80, 0, 100)

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
    println("\nTask 4")
    print("a. ")
    println(getFoldLDays(weekdays))
    print("b. ")
    println(getFoldRDays(weekdays))
    print("c. ")
    println(foldLSDays(weekdays))
    println("\nTask 5")
    println(getReducedPrices(goods))
    println("\nTask 6")
    println("Original list: " + ints)
    println("Incremented list: " + getIncrementedInts(ints))
    println("\nTask 7")
    println("Original list: " + naturalNumbers)
    println("Trimmed list of absolute values: " + getAbsoluteValues(naturalNumbers))
    println("\nTask 8")
    println(printTuple(tuple))
    println("\nTask 9")
    println("Original list with zero values: " + intsZeroes)
    println("Trimmed list: " + removeZeroes(intsZeroes))
    println("\nTask 10")
    print("a. Returning 'None' for a non-existent map key - ")
    val str: String = "sandwich"
    println(s"corresponding value for '$str' - " + goods.get(str))
    print(s"b. Using pattern matching to return message for a non-existent map key: value for '$str' is - " + optionViaPattern(goods.get(str)))
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
    }
    else {
      weekdays.head + getWeekdaysRecursion(weekdays.tail)
    }
  }

  def getWeekdaysRecursionRev(weekdays: List[String]): String = {
    if (weekdays.isEmpty)
      ""
    else if (!weekdays.head.equals(weekdays.last)) {
      getWeekdaysRecursionRev(weekdays.tail) + ", " + weekdays.head
    }
    else {
      getWeekdaysRecursionRev(weekdays.tail) + weekdays.head
    }
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

  def getFoldLDays(weekdays : List[String]) : String = {
    weekdays.foldLeft("")((m, n ) => m + ", " +n)
  }

  def getFoldRDays(weekdays : List[String]) : String = {
    weekdays.foldRight("")((m, n ) => n + ", " +m)
  }

  def foldLSDays(weekdays : List[String]) : String = {
    weekdays.foldLeft(""){
      case (acc, s"S$nme") => acc + s"S$nme" + ", "
      case (acc, _ ) => acc
    }
  }

  def getIncrementedInts(ints: List[Int]): List[Int] = {
    ints.map(n => n+1)
  }

  def getReducedPrices(goods : Map[String, Double]) : Map [String, Double] = {
    goods.map(kv => (kv._1, kv._2 * 0.9))
  }

  def getAbsoluteValues(natNumbers: List[Int]) : List[Int] = {
    val x = natNumbers.filter { num =>
        num >= -5 && num <= 12
      }
    x.map(_.abs)
  }

  def printTuple(tuple : (Int, String, Double)) {
    print("Iterating through tuple: ")
    tuple.productIterator.foreach {
      i => print(i + ", ")
    }
    println
    print("Yet another way: ")
    print(tuple._1, tuple._2, tuple._3)
  }

  def removeZeroes(list: List[Int]): List[Int] = list match {
    case x :: xs if x == 0 => removeZeroes(xs)
    case x :: xs => x :: removeZeroes(xs)
    case _ => Nil
  }

  def optionViaPattern(z: Option[Double]): Any = z match
  {
    case Some(s) => s
    case None => "no such key"
  }
}
