package quanto.core.protocol.userdata;

import quanto.core.protocol.ProtocolManager;
import quanto.core.protocol.userdata.dataserialization.Point2DDataSerializer;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */
public class PositionGraphUserDataSerializer extends GraphUserDataSerializer {

    private static String suffix = "position";
    private static Point2DDataSerializer serializer = new Point2DDataSerializer();

    public PositionGraphUserDataSerializer(ProtocolManager talker) {
        super(talker, serializer, suffix);
        System.out.println("Initialization " + suffix);
    }
}
