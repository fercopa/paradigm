object scala_test_5 {

    def factorial(n: Int): Int = {
        if (n <= 1) return 1/0;
        else return n*factorial(n - 1);
    }

    def main (args: Array[String]) {
        try { 
            println (factorial(5));
        } catch {
            case e: Exception => e.printStackTrace;
            System.exit(1);
        }
    }
}
