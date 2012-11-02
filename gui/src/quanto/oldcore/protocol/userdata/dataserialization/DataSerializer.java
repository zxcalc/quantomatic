package quanto.oldcore.protocol.userdata.dataserialization;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */

/*
    Convert data to and from String
*/
public interface DataSerializer<D> {
    public D fromString(String data);
    public String toString(D data);
}
