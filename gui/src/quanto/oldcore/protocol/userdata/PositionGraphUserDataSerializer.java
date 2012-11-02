package quanto.oldcore.protocol.userdata;

import java.awt.geom.Point2D;
import quanto.oldcore.protocol.CoreTalker;
import quanto.oldcore.protocol.userdata.dataserialization.Point2DDataSerializer;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */
public class PositionGraphUserDataSerializer extends GraphUserDataSerializer<Point2D> {

    private static String suffix = "position";
    private static Point2DDataSerializer serializer = new Point2DDataSerializer();

    public PositionGraphUserDataSerializer(CoreTalker talker) {
        super(talker, serializer, suffix);
    }
}
