import java.util.*;
import java.lang.*;
import java.io.*;


class test {
    static int factorial(int n) {
        if (n == 0) {
            for (StackTraceElement ste : Thread.currentThread().getStackTrace()) 
            {
                System.out.println(ste);
            }
            return 1;
        }
        else {
            return n * factorial(n-1);
        }
    }

public static void main(String[] args) {
        System.out.println(factorial(5));
    }
}
