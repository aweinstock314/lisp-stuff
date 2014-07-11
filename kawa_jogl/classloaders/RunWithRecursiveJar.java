import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;

public class RunWithRecursiveJar
{
    public static void asynchStreamCopy(final InputStream is, final OutputStream os) { asynchStreamCopy(is,os,0x1000); }
    public static void asynchStreamCopy(final InputStream is, final OutputStream os, final int BUF_SIZE)
    {
        Thread t = new Thread(new Runnable(){
            @Override public void run()
            {
                try
                {
                    int rc;
                    byte[] buf = new byte[BUF_SIZE];
                    while((rc = is.read(buf,0,BUF_SIZE)) != -1)
                    {
                        os.write(buf,0,rc);
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

    public static void dumpRecursiveJars() throws IOException
    {
        RecursiveJarUtil.MAKE_TEMPORARIES = false;
        System.out.printf("export CLASSPATH=%s",RecursiveJarUtil.getRecursivelyExpandedClasspath());
    }

    public static void main(String[] args) throws Throwable
    {
        // dump option intended to be invoked as "$(java RunWithRecuriveJar --dump)" from bash (with recursive jars on the classpath), for efficiency during development
        if(args.length > 0 && args[0].equals("--dump")) { dumpRecursiveJars(); }
        else { System.exit(invokeWithRedirection(argsForJavaWithExtendedClasspath(args))); }
    }
}
