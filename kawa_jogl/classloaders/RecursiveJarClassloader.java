import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;

public class RecursiveJarClassloader extends ClassLoader
{
    private static final int BUF_SIZE = 0x1000;
    private static int counter = 0;
    public RecursiveJarClassloader() { super(); }
    private HashSet<String> opened_jarnames = new HashSet<String>();
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

    public static void extractSubjarsToDirectory(File dir, String name) throws IOException
    {
        if(name.endsWith(".jar"))
        {
            extractSubjarsToDirectory(dir,new JarInputStream(new FileInputStream(name)));
        }
    }
    public static void extractSubjarsToDirectory(File dir, JarInputStream jis) throws IOException
    {
        byte[] buf = new byte[BUF_SIZE];
        int rc;
        for(JarEntry je;(je = jis.getNextJarEntry()) != null;)
        {
            if(!je.getName().endsWith(".jar"))continue;
            try
            {
                File outputJar = new File(dir,je.getName());
                outputJar.deleteOnExit();
                FileOutputStream fos = new FileOutputStream(outputJar);
                while((rc = jis.read(buf,0,BUF_SIZE)) != -1) { fos.write(buf,0,rc); }
                fos.close();
                System.out.printf("RecursiveJarClassloader: Extracting %s to %s\n",je.getName(),outputJar.getAbsolutePath());
                extractSubjarsToDirectory(dir,outputJar.getAbsolutePath());
            } catch(IOException ioe) {continue;}
        }
    }

    public static void main(String[] args) throws Throwable
    {
        File tmpdir = makeTempdir();
        for(String cpe : getClasspathEntries()) 
        {
            extractSubjarsToDirectory(tmpdir,cpe);
        }
    }
}
