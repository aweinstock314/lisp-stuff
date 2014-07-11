import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;

public class RunWithRecursiveJar
{
    public static void asynchStreamCopy(final InputStream is, final OutputStream os)
    {
        Thread t = new Thread(new Runnable(){
            @Override public void run()
            {
                try
                {
                    int rc;
                    while((rc = is.read()) != -1)
                    {
                        os.write(rc);
                        os.flush();
                    }
                    os.close();
                }
                catch(IOException ioe) {}
            }
        });
        t.setDaemon(true);
        t.start();
    }

    public static int invokeWithRedirection(ArrayList<String> args) throws IOException, InterruptedException
    {
        Process p = Runtime.getRuntime().exec(args.toArray(new String[0]));
        asynchStreamCopy(System.in,p.getOutputStream());
        asynchStreamCopy(p.getInputStream(),System.out);
        asynchStreamCopy(p.getErrorStream(),System.err);
        return p.waitFor();
    }

    public static ArrayList<String> argsForJavaWithExtendedClasspath(String[] classnameAndArgs) throws IOException
    {
        String expandedCP = RecursiveJarUtil.getRecursivelyExpandedClasspath();
        ArrayList<String> args = new ArrayList<String>();
        args.add("java");
        args.add("-cp");
        args.add(expandedCP);
        for(String arg : classnameAndArgs) { args.add(arg); }
        return args;
    }

    public static void main(String[] args) throws Throwable
    {
        System.exit(invokeWithRedirection(argsForJavaWithExtendedClasspath(args)));
    }
}
