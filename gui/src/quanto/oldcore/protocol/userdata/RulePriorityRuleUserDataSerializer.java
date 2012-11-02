package quanto.oldcore.protocol.userdata;

import quanto.oldcore.protocol.CoreTalker;
import quanto.oldcore.protocol.userdata.dataserialization.IntegerDataSerializer;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */
public class RulePriorityRuleUserDataSerializer extends RuleUserDataSerializer<Integer> {

    static String suffix = "priority";
    static IntegerDataSerializer serializer = new IntegerDataSerializer();

    public RulePriorityRuleUserDataSerializer(CoreTalker talker) {
        super(talker, serializer, suffix);
    }
}
