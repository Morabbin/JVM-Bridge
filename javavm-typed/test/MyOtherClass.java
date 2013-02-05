public class MyOtherClass
    {
    private MyClass parent;

    public MyOtherClass(MyClass par)
        {
        parent = par;
        };
    
    public void showParentID()
        {
        System.out.print("Parent ID is "+Integer.toString(parent.getID())+"\n");
        };
    };
