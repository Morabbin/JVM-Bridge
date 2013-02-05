public class MyClass
    {
    private static int n = 0;
    
    private int id;

    public MyClass()
        {
        id = n;
        n = n + 1;
        };

    public MyOtherClass makeOtherClass()
        {
        return new MyOtherClass(this);
        };

    public int getID()
        {
        return id;
        };

    public static double square (double x)
        {
        System.out.print("The square of "+Double.toString(x)+"\n");
        double y = x * x;
        System.out.print("is "+Double.toString(y)+"\n");
        return y;
        };
    
    public static void sayHello ()
        {
        System.out.print("Hello\n");
        (new MyClass()).makeOtherClass().showParentID();
        (new MyClass()).makeOtherClass().showParentID();
        };
    };
