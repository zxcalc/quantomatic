package quanto.core.protocol.userdata;

import quanto.core.protocol.ProtocolManager;
import quanto.core.protocol.userdata.dataserialization.StringDataSerializer;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */
public class CopyOfGraphUserDataSerializer extends GraphUserDataSerializer {

    private static String suffix = "copy_of";
    private static StringDataSerializer serializer = new StringDataSerializer();

    public CopyOfGraphUserDataSerializer(ProtocolManager talker) {
        super(talker, serializer, suffix);
    }
}
