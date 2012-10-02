package quanto.core;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import quanto.core.data.VertexType;
import quanto.core.data.xml.TheoryHandler;

/**
 * A theory description.
 *
 * Describes the vertices (including how to visualise them) of a theory, and
 * links it to a core theory that actually implements it.
 *
 * NB: Theory objects are partly immutable in order to
 * prevent data being changed without the Core being aware of it.
 *
 * @author Alex Merry
 */
public class Theory {
    /* immutable: used by Core */
    private final String coreName;
    /* immutable: used by Core */
    private final Map<String, VertexType> types;

    protected String friendlyName;
    protected Map<Character, VertexType> mnemonics = new HashMap<Character, VertexType>();

    public Theory(TheoryHandler.Data data) {
        this(data.coreName, data.name, data.vertices);
    }

    public Theory(String coreName, String friendlyName, Collection<VertexType> types) {
        this.coreName = coreName;
        this.friendlyName = friendlyName;
        Map<String, VertexType> vTypeMap = new HashMap<String, VertexType>();
        for (VertexType vt : types) {
            vTypeMap.put(vt.getTypeName(), vt);
            if (vt.getMnemonic() != null) {
                this.mnemonics.put(vt.getMnemonic(), vt);
            }
        }
        this.types = Collections.unmodifiableMap(vTypeMap);
    }

    /**
     * Get the vertex type with a given name.
     * 
     * This can be used to get the vertex type data given a vertex type name
     * as returned by the core.
     *
     * @param typeName the (core) name for a vertex type
     * @return a vertex type if one exists with typeName, otherwise null
     */
    public VertexType getVertexType(String typeName) {
        return types.get(typeName);
    }

    /**
     * Gets the vertex type associated with a mnemonic, if there is one.
     *
     * @param mnemonic the key typed by the user
     * @return a vertex type, or null
     */
    public VertexType getVertexTypeByMnemonic(char mnemonic) {
        return mnemonics.get(mnemonic);
    }

    /**
     * The available vertex types.
     * 
     * This should match the core theory's list of vertices.
     *
     * @return an unmodifiable collection of vertex type descriptions
     */
    public Collection<VertexType> getVertexTypes() {
        return types.values();
    }

    /**
     * The core theory that is used by this theory.
     *
     * @return a core theory name
     */
    public String getCoreName() {
        return this.coreName;
    }

    /**
     * The name that should be presented to the user.
     *
     * @return a name that can be presented to the user
     */
    public String getFriendlyName() {
        return friendlyName;
    }

    /**
     * A string representation of the theory.
     *
     * @return  the same as getFriendlyName()
     */
    @Override
    public String toString() {
        return friendlyName;
    }
}
