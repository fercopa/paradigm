object Main extends App {
    var y = 3;
    lazy val x = { System.nanoTime }
    println("call-by-name")
    mostrar(prod(y))
    println("call-by-value usando lazy")
    mostrar(x)
    println("Vuelvo a llamar a ambos")
    mostrar(prod(y))
    mostrar(x)
    
    def prod(a: =>Int) = { System.nanoTime }
    
    def mostrar(t: =>Long) = { println("Parametro: " + t) }
}
