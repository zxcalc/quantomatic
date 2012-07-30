package quanto.core.protocol.userdata;

import quanto.core.CoreException;
import quanto.core.protocol.ProtocolManager;
import quanto.core.protocol.userdata.dataserialization.DataSerializer;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */
public class RuleUserDataSerializer<T> extends QuantoAppUserDataSerializer<T> {

    private String suffix;
    private DataSerializer serializer;
    private final static Logger logger = Logger.getLogger("quanto.protocol.userdata");

    public RuleUserDataSerializer(ProtocolManager talker, DataSerializer<T> serializer, String suffix) {
        super(talker);
        this.suffix = suffix;
        this.serializer = serializer;
    }

    public void setRuleUserData(String ruleName, T data) {
        String dataString = serializer.toString(data);
        if (dataString == null)
            return;
        try {
            talker.setRuleUserData(ruleName, prefix + suffix,dataString);
        } catch (CoreException e) {
            logger.log(Level.WARNING, "Could not set user data " + prefix + suffix + " on " +
                    "rule " + ruleName);
        }
    }

    public T getRuleUserData(String ruleName) {
        try {
            return (T) serializer.fromString(talker.ruleUserData(ruleName, prefix + suffix));
        } catch (CoreException e) {
            logger.log(Level.WARNING, "Could not get user data " + prefix + suffix + " on " +
                    " rule " + ruleName);
            return null;
        }
    }
}
