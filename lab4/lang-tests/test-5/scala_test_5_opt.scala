import scala.annotation.tailrec
object scala_test_5_opt {

    @tailrec def factorialAcc(acc: Int, n: Int): Int = {
        if (n <= 1) return 1/0;
        else return factorialAcc(n * acc, n - 1);
    }

    def main (args: Array[String]) {
        try { 
            println (factorialAcc(1, 5));
        } catch {
            case e: Exception => e.printStackTrace;
            System.exit(1);
        }
    }
}
