import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.security.AllPermission;
import java.security.CodeSource;
import java.security.PermissionCollection;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;

public class RecursiveJarClassloader extends URLClassLoader
{
    private static final int BUF_SIZE = 0x1000;
    private static int counter = 0;
    public static String[] getClasspathEntries()
    {
        return System.getProperty("java.class.path").split(System.getProperty("path.separator"));
    }

    public static File makeTempdir() throws IOException
    {
        File tmpdir = new File("temp/",String.format("RJCRdir%d-%d", ++counter, System.currentTimeMillis()));
        if(!tmpdir.mkdirs())throw new IOException("Couldn't create temp directory "+tmpdir.getAbsolutePath());
        tmpdir.deleteOnExit();
        return tmpdir;
    }

    public static ArrayList<File> extractSubjarsToDirectory(File dir, String name) throws IOException
    {
        if(name.endsWith(".jar"))
        {
            return extractSubjarsToDirectory(dir,new JarInputStream(new FileInputStream(name)));
        }
        return new ArrayList<File>();
    }
    public static ArrayList<File> extractSubjarsToDirectory(File dir, JarInputStream jis) throws IOException
    {
        ArrayList<File> jars = new ArrayList<File>();
        byte[] buf = new byte[BUF_SIZE];
        int rc;
        for(JarEntry je;(je = jis.getNextJarEntry()) != null;)
        {
            if(!je.getName().endsWith(".jar"))continue;
            try
            {
                File outputJar = new File(dir,je.getName());
                jars.add(outputJar);
                outputJar.deleteOnExit();
                FileOutputStream fos = new FileOutputStream(outputJar);
                while((rc = jis.read(buf,0,BUF_SIZE)) != -1) { fos.write(buf,0,rc); }
                fos.close();
                //System.out.printf("RecursiveJarClassloader: Extracting %s to %s\n",je.getName(),outputJar.getAbsolutePath());
                jars.addAll(extractSubjarsToDirectory(dir,outputJar.getAbsolutePath()));
            } catch(IOException ioe) {continue;}
        }
        return jars;
    }

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
        String expandedCP = getRecursivelyExpandedClasspath();
        ArrayList<String> newargs = new ArrayList<String>();
        newargs.add("java");
        newargs.add("-cp");
        newargs.add(expandedCP);
        for(String arg : args) { newargs.add(arg); }
        Process p = Runtime.getRuntime().exec(newargs.toArray(new String[0]));
        //System.setIn(p.getInputStream());
        asynchStreamCopy(System.in,p.getOutputStream());
        asynchStreamCopy(p.getInputStream(),System.out);
        asynchStreamCopy(p.getErrorStream(),System.err);
        //System.setOut(new java.io.PrintStream(p.getOutputStream()));
        p.waitFor();
        System.out.println("Hello, world!");
        //int rc;
        //while((rc = p.getInputStream().read()) != -1)System.out.print(Character.valueOf((char)rc));
    }

    public static String getRecursivelyExpandedClasspath()
    {
        StringBuilder buf = new StringBuilder(System.getProperty("java.class.path"));
        String pathSep = System.getProperty("path.separator");
        try
        {
            File tmpdir = makeTempdir();
            for(String cpe : getClasspathEntries()) 
            {
                try
                {
                    for(File newjar : extractSubjarsToDirectory(tmpdir,cpe))
                    {
                        buf.append(pathSep);
                        buf.append(newjar.getAbsolutePath());
                    }
                }
                catch(IOException ioe) {continue;}
            }
        }
        catch(IOException ioe) {}
        return buf.toString();
    }

    public static URL[] getRecursivelyExpandedClasspathURLs()
    {
        try
        {
            ArrayList<URL> urls = new ArrayList<URL>();
            File tmpdir = makeTempdir();
            for(String cpe : getClasspathEntries()) 
            {
                try
                {
                    for(File newjar : extractSubjarsToDirectory(tmpdir,cpe))
                    {
                        try {urls.add(newjar.toURI().toURL());}
                        catch(java.net.MalformedURLException e) {continue;}
                    }
                }
                catch(IOException ioe) {continue;}
            }
            return urls.toArray(new URL[0]);
        }
        catch(IOException ioe) {return new URL[0];}
    }

    public RecursiveJarClassloader()
    {
        super(getRecursivelyExpandedClasspathURLs());
    }
////    @Override
////    public PermissionCollection getPermissions(CodeSource cs)
////    {
////        System.out.println("hello, world");
////        PermissionCollection pc = super.getPermissions(cs);
////        pc.add(new AllPermission());
////        return pc;
////    }
//    public RecursiveJarClassloader()
//    {
//        super();
//        //System.setProperty("java.class.path",getRecursivelyExpandedClasspath());
//        //System.out.println(System.getProperty("java.class.path"));
//    }
//
//    @Override
//    public Class<?> findClass(String name, boolean resolve)
//    {
//        
//    }
}
