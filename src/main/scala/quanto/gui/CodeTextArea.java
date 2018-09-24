package quanto.gui;

import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.textarea.*;

import java.util.Properties;


public class CodeTextArea extends StandaloneTextArea {
    static private IPropertyManager propertyManager;
    static {
        final Properties props = new Properties();

        propertyManager = new IPropertyManager() {
            public String getProperty(String prop) { return props.getProperty(prop); }
        };
    }

    public CodeTextArea() { super(propertyManager); }
}
