package quanto.oldcore.protocol.userdata;

import quanto.oldcore.protocol.CoreTalker;
import quanto.oldcore.protocol.userdata.dataserialization.StringDataSerializer;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */
public class CopyOfGraphUserDataSerializer extends GraphUserDataSerializer<String> {

    private static String suffix = "copy_of";
    private static StringDataSerializer serializer = new StringDataSerializer();

    public CopyOfGraphUserDataSerializer(CoreTalker talker) {
        super(talker, serializer, suffix);
    }
}
