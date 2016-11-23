import java.util.*;
import java.lang.*;
import java.io.*;


class test2 {
    static int factorial(int n, int acc) {
        if (n == 0) {
            for (StackTraceElement ste : Thread.currentThread().getStackTrace()) 
            {
                System.out.println(ste);
            }
            return acc;
        }
        else {
            return factorial(n-1, n*acc);
        }
    }

public static void main(String[] args) {
        System.out.println(factorial(5, 1));
    }
}
