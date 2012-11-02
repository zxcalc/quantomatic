package quanto.oldcore.protocol.userdata;

import java.util.logging.Level;
import java.util.logging.Logger;
import quanto.core.CoreException;
import quanto.oldcore.protocol.CoreTalker;
import quanto.oldcore.protocol.userdata.dataserialization.DataSerializer;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */
public class RuleUserDataSerializer<T> extends QuantoAppUserDataSerializer<T> {

    private String suffix;
    private DataSerializer<T> serializer;
    private final static Logger logger = Logger.getLogger("quanto.protocol.userdata");

    public RuleUserDataSerializer(CoreTalker talker, DataSerializer<T> serializer, String suffix) {
        super(talker);
        this.suffix = suffix;
        this.serializer = serializer;
    }

    public void setRuleUserData(String ruleName, T data) {
        String dataString = serializer.toString(data);
        if (dataString == null) {
			return;
		}
        try {
            talker.setRuleUserData(ruleName, prefix + suffix,dataString);
        } catch (CoreException e) {
            logger.log(Level.WARNING, "Could not set user data " + prefix + suffix + " on " +
                    "rule " + ruleName, e);
        }
    }

    public T getRuleUserData(String ruleName) {
        try {
            return serializer.fromString(talker.ruleUserData(ruleName, prefix + suffix));
        } catch (CoreException e) {
            logger.log(Level.WARNING, "Could not get user data " + prefix + suffix + " on " +
                    " rule " + ruleName, e);
            return null;
        }
    }
}
