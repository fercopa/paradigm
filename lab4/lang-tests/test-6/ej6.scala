object ej6 {
    var y = 6
    lazy val x = 1 + y
    println(x)
    y = 0
    println(x)
    println(y)
}
