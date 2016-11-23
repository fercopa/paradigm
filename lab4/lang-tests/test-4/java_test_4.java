class java_test_4 {
    String x = "estatico";
    void g(){
        System.out.println(x);
    }
    void f(){
        String x = "dinamico";
        g();
    }
    public static void main (String[] args){
        String x = "estatico";
        java_test_4 a = new java_test_4();
        a.f();
    }
}
