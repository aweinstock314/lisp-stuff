import java.net.URLClassLoader;
import java.security.AllPermission;
import java.security.CodeSource;
import java.security.PermissionCollection;

public class RecursiveJarClassloader extends URLClassLoader
{
    public RecursiveJarClassloader()
    {
        super(RecursiveJarUtil.getRecursivelyExpandedClasspathURLs());
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
