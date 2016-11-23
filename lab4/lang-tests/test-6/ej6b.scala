object ej6b {
    var y = 6
    var z = 1
    def sum (): Int =
        y + z
    lazy val lzy = sum _
    println (lzy)
    println (lzy())
    y = 7
    z = 2
    println (lzy())
}
