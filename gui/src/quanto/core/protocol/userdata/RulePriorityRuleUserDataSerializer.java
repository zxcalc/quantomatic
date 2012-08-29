package quanto.core.protocol.userdata;

import quanto.core.protocol.ProtocolManager;
import quanto.core.protocol.userdata.dataserialization.IntegerDataSerializer;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */
public class RulePriorityRuleUserDataSerializer extends RuleUserDataSerializer<Integer> {

    static String suffix = "priority";
    static IntegerDataSerializer serializer = new IntegerDataSerializer();

    public RulePriorityRuleUserDataSerializer(ProtocolManager talker) {
        super(talker, serializer, suffix);
    }
}
