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
                }
                catch(IOException ioe) {}
            }
        });
        t.setDaemon(true);
        t.start();
    }

    public static void main(String[] args) throws Throwable
    {
        String expandedCP = RecursiveJarUtil.getRecursivelyExpandedClasspath();
        ArrayList<String> newargs = new ArrayList<String>();
        newargs.add("java");
        newargs.add("-cp");
        newargs.add(expandedCP);
        for(String arg : args) { newargs.add(arg); }
        Process p = Runtime.getRuntime().exec(newargs.toArray(new String[0]));
        asynchStreamCopy(System.in,p.getOutputStream());
        asynchStreamCopy(p.getInputStream(),System.out);
        asynchStreamCopy(p.getErrorStream(),System.err);
        p.waitFor();
    }
}
