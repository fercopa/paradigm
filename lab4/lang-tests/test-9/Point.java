public class Point { //call by value
    public int x;
    public int y;
    public Point(int x, int y){
        this.x = x;
        this.y = y;
    }
    public static void tricky1(Point arg1, Point arg2)
    {
        arg1.x = 100;
        arg1.y = 100;
        Point temp = arg1;
        arg1 = arg2;
        arg2 = temp;
    }
    public static void tricky2(Point arg1, Point arg2)
    {
        arg1 = null;
        arg2 = null;
    }
    public static void main(String [] args)
    {
        Point pnt1 = new Point(0,0);
        Point pnt2 = new Point(0,0);
        System.out.println("\npnt1 X: " + pnt1.x + " pnt1 Y: " + pnt1.y);
        System.out.println("pnt2 X: " + pnt2.x + " pnt2 Y: " + pnt2.y);
        System.out.println("\ntriki1");
        tricky1(pnt1,pnt2);
        System.out.println("pnt1 X: " + pnt1.x + " pnt1 Y: " + pnt1.y);
        System.out.println("pnt2 X: " + pnt2.x + " pnt2 Y: " + pnt2.y);
        System.out.println("\ntriki2");
        tricky2(pnt1,pnt2);
        System.out.println("pnt1 X: " + pnt1.x + " pnt1 Y: " + pnt1.y);
        System.out.println("pnt2 X: " + pnt2.x + " pnt2 Y: " + pnt2.y + "\n");
    }
}

