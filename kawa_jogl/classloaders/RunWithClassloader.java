import java.lang.reflect.Method;

public class RunWithClassloader extends ClassLoader
{
    public static void printUsageAndQuit(String[] args)
    {
        System.out.println("Usage: java RunWithClassLoader CLASSLOADER MAINCLASS [ARGS ...]");
        System.exit(0);
    }

    public static void main(String[] args) throws Throwable
    {
        if(args.length < 2) { printUsageAndQuit(args); }
        String classloader_name = args[0];
        String main_class_name = args[1];
        String[] rest_of_args = java.util.Arrays.copyOfRange(args,2,args.length);
        ClassLoader cl = (ClassLoader)Class.forName(classloader_name).getConstructor().newInstance();
        Class<?> StringArray = (new String[0]).getClass();
        Method forwarding_main = cl.loadClass(main_class_name).getMethod("main",StringArray);
        forwarding_main.invoke(null,new Object[]{rest_of_args});
    }
    public RunWithClassloader() { super(); }
}

