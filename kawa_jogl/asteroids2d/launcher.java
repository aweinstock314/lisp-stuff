public class launcher
{
    public static void main(String[] args) throws Throwable
    {
        // maybe have this sort of class auto-generated with gnu.bytecode in the future?
        String[] newargs = new String[args.length+1];
        System.arraycopy(args,0,newargs,1,args.length);
        newargs[0] = "asteroids2d";
        RunWithRecursiveJar.main(newargs);
    }
}
