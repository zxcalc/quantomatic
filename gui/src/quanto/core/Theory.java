package quanto.core;

import com.fasterxml.jackson.core.JsonEncoding;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.lindenb.awt.ColorUtils;
import org.lindenb.lang.InvalidXMLException;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;
import quanto.core.data.GraphElementDataType;
import quanto.core.data.SVGDocument;
import quanto.core.data.SvgVertexVisualizationData;
import quanto.core.data.VertexType;

/**
 * A theory description.
 *
 * Describes the vertices (including how to visualise them) of a theory, and
 * links it to a core theory that actually implements it.
 *
 * NB: Theory objects are partly immutable in order to prevent data being
 * changed without the Core being aware of it.
 *
 * @author Alex Merry
 */
public class Theory {

	private String name;
	private String coreName;
	private String vertexTypePath;
	private Map<String, VertexType> vertexTypes;
	private Map<Character, VertexType> mnemonics = new HashMap<Character, VertexType>();

	public Theory(
			String coreName,
			String name,
			String vertexTypePath,
			Collection<VertexType> types) {

		if (types.isEmpty()) {
			throw new IllegalArgumentException("No vertex types given");
		}
		if (vertexTypePath == null && types.size() != 1) {
			throw new IllegalArgumentException("No vertex type path, but more than one vertex type given");
		}

		this.coreName = coreName;
		this.name = name;
		this.vertexTypePath = vertexTypePath;
		Map<String, VertexType> vTypeMap = new HashMap<String, VertexType>();
		for (VertexType vt : types) {
			vTypeMap.put(vt.getTypeName(), vt);
			if (vt.getMnemonic() != null) {
				this.mnemonics.put(vt.getMnemonic(), vt);
			}
		}
		this.vertexTypes = Collections.unmodifiableMap(vTypeMap);
	}

	public Theory(
			String coreName,
			String name,
			Collection<VertexType> types) {
		this(coreName, name, null, types);
	}

	/**
	 * Get the vertex type from JSON vertex data.
	 *
	 * This can be used to get the vertex type data given a vertex type name as
	 * returned by the core.
	 *
	 * @param typeName the (core) name for a vertex type
	 * @return a vertex type if one exists with typeName, otherwise null
	 */
	public VertexType getVertexType(JsonNode data) {
		if (vertexTypePath != null) {
			JsonNode typeNode = data;
			if (vertexTypePath.length() > 0)
				typeNode = data.get(vertexTypePath);
			if (typeNode == null || !typeNode.isTextual()) {
				throw new IllegalArgumentException("Data did not have a type selector at '" + vertexTypePath + "'");
			}
			String typeName = typeNode.asText();
			return getVertexType(typeName);
		} else {
			return vertexTypes.values().iterator().next();
		}
	}

	/**
	 * Get the vertex type with a given name.
	 *
	 * This can be used to get the vertex type data given a vertex type name as
	 * returned by the core.
	 *
	 * @param typeName the (core) name for a vertex type
	 * @return a vertex type if one exists with typeName, otherwise null
	 */
	public VertexType getVertexType(String typeName) {
		VertexType type = vertexTypes.get(typeName);
		if (type == null) {
			throw new IllegalArgumentException("Unknown vertex type '" + typeName + "'");
		}
		return type;
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
		return vertexTypes.values();
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
	 * The name of the theory.
	 *
	 * This is a user-presentable string.
	 *
	 * @return a name that can be presented to the user
	 */
	public String getName() {
		return name;
	}

	/**
	 * A string representation of the theory.
	 *
	 * @return the same as getFriendlyName()
	 */
	@Override
	public String toString() {
		return name;
	}

	private static VertexType vertexTypeFromJson(URL contextUrl, String name, JsonNode node) throws ParseException {
		if (!node.isObject())
			throw new ParseException("Vertex description for \"" + name + "\" was not an object");
		VertexType vt = new VertexType(name);

		JsonNode labelPathNode = node.get("labelPath");
		if (labelPathNode != null && !labelPathNode.isNull()) {
			if (!labelPathNode.isTextual()) {
				throw new ParseException("'labelPath' was not a string");
			}
			JsonNode labelTypeNode = node.get("labelDataType");
			if (labelTypeNode != null && !labelTypeNode.isNull()) {
				if (!labelTypeNode.isTextual()) {
					throw new ParseException("'labelDataType' was not a string");
				}
				String labelDataType = labelTypeNode.asText();
				if (labelDataType.equals("MathExpression")) {
					vt.setDataType(new GraphElementDataType.MathsData(labelPathNode.asText()));
				} else if (labelDataType.equals("String")) {
					vt.setDataType(new GraphElementDataType.StringData(labelPathNode.asText()));
				} else if (!labelDataType.equals("Null")) {
					throw new ParseException("Unknown label data type \"" + labelDataType + "\"");
				}
			} else {
				vt.setDataType(new GraphElementDataType.StringData(labelPathNode.asText()));
			}
		}

		JsonNode mnemonicNode = node.get("mnemonic");
		if (mnemonicNode != null && !mnemonicNode.isNull()) {
			if (!mnemonicNode.isTextual()) {
				throw new ParseException("'mnemonic' was not a string");
			}
			String mnemonicStr = mnemonicNode.asText();
			if (mnemonicStr.length() != 1) {
				throw new ParseException("'mnemonic' was not exactly one character long");
			}
			vt.setMnemonic(mnemonicStr.charAt(0));
		}

		JsonNode visNode = node.get("visualization");
		if (visNode == null || !visNode.isObject()) {
			throw new ParseException("'visualization' did not exist or was not an object");
		}
		JsonNode svgNode = visNode.get("node");
		if (svgNode == null || !svgNode.isTextual()) {
			throw new ParseException("'visualization.node' did not exist or was not a string");
		}
		SVGDocument svgdoc;
		try {
			URL svgURL = new URL(contextUrl, svgNode.asText());
			svgdoc = new SVGDocument(svgURL);
		} catch (MalformedURLException e) {
			throw new ParseException("Malformed URL for SVG file", e);
		} catch (InvalidXMLException e) {
			throw new ParseException("Malformed SVG file", e);
		} catch (SAXException e) {
			throw new ParseException("Malformed SVG file", e);
		} catch (IOException e) {
			throw new ParseException("Could not open SVG file \"" + svgNode.asText() + "\"", e);
		}
		JsonNode labelNode = visNode.get("label");
		Color labelFill = null;
		if (labelNode != null && !labelNode.isNull()) {
			if (!labelNode.isObject()) {
				throw new ParseException("'visualization.label' was not an object");
			}
			JsonNode labelFillNode = labelNode.get("fill");
			if (labelFillNode != null && !labelFillNode.isNull()) {
				if (!labelFillNode.isTextual()) {
					throw new ParseException("'visualization.label.fill' was not a string");
				}
				labelFill = ColorUtils.parseColor(labelFillNode.asText());
				if (labelFill == null) {
					throw new ParseException("'visualization.label.fill' was not a valid colour");
				}
			}
		}
		vt.setVisualizationData(new SvgVertexVisualizationData(svgdoc, labelFill));

		return vt;
	}

	private static Theory fromJson(URL contextUrl, JsonNode node) throws ParseException {
		JsonNode nameNode = node.get("name");
		if (nameNode == null || !nameNode.isTextual()) {
			throw new ParseException("Theory had no 'name' entry");
		}

		JsonNode coreNameNode = node.get("coreName");
		if (coreNameNode == null || !coreNameNode.isTextual()) {
			throw new ParseException("Theory had no 'coreName' entry");
		}

		JsonNode vertexTypesNode = node.get("vertexTypes");
		if (vertexTypesNode == null || !vertexTypesNode.isObject()) {
			throw new ParseException("Theory had no 'vertexTypes' entry");
		}

		LinkedList<VertexType> vtList = new LinkedList<VertexType>();
		Iterator<Map.Entry<String, JsonNode>> it = vertexTypesNode.fields();
		while (it.hasNext()) {
			Map.Entry<String, JsonNode> entry = it.next();
			vtList.add(vertexTypeFromJson(contextUrl, entry.getKey(), entry.getValue()));
		}
		if (vtList.isEmpty()) {
			throw new ParseException("'vertexTypes' was empty");
		}

		JsonNode vertexTypePathNode = node.get("vertexTypePath");
		if (vertexTypePathNode != null && !vertexTypePathNode.isNull()) {
			if (!vertexTypePathNode.isTextual()) {
				throw new ParseException("'vertexTypePath' was not a string");
			}
			return new Theory(coreNameNode.asText(),
					nameNode.asText(),
					vertexTypePathNode.asText(),
					vtList);
		} else {
			if (vtList.size() != 1) {
				throw new IllegalArgumentException("No vertex type path, but more than one vertex type given");
			}
			return new Theory(coreNameNode.asText(),
					nameNode.asText(),
					vtList);
		}
	}
	private static final JsonFactory jf = new JsonFactory();

	public static Theory fromFile(File theoryFile) throws IOException, ParseException {
		try {
			ObjectMapper jsonMapper = new ObjectMapper(jf);
			JsonNode node = jsonMapper.readTree(theoryFile);
			return fromJson(theoryFile.toURI().toURL(), node);
		} catch (MalformedURLException ex) {
			throw new IllegalArgumentException("theoryFile cannot be converted to a URL", ex);
		}
	}

	public static Theory fromUrl(URL theoryFile) throws IOException, ParseException {
		ObjectMapper jsonMapper = new ObjectMapper(jf);
		JsonNode node = jsonMapper.readTree(theoryFile);
		return fromJson(theoryFile, node);
	}

	private void writeXMLResource(Document resource, File dest) throws IOException {
		try {
			// Use a Transformer for output
			TransformerFactory tFactory =
					TransformerFactory.newInstance();
			Transformer transformer = tFactory.newTransformer();

			DOMSource source = new DOMSource(resource);
			StreamResult result = new StreamResult(dest);
			transformer.transform(source, result);
		} catch (TransformerException ex) {
			throw new IOException("Could not write out the file");
		}
	}

	public void write(File theoryFile, File resourceDirectory) throws IOException {
		if (!resourceDirectory.isDirectory()) {
			throw new IOException("\"" + resourceDirectory.toString() + "\" is not a directory");
		}
		boolean resDirIsTheoryParent = resourceDirectory.equals(theoryFile.getParentFile());

		JsonGenerator jg = jf.createGenerator(theoryFile, JsonEncoding.UTF8);
		try {
			jg.writeStartObject();
			jg.writeObjectField("name", name);
			jg.writeObjectField("coreName", coreName);
			if (vertexTypePath != null) {
				jg.writeObjectField("vertexTypePath", vertexTypePath);
			}
			jg.writeFieldName("vertexTypes");
			jg.writeStartObject();
			for (VertexType vt : vertexTypes.values()) {
				jg.writeFieldName(vt.getTypeName());
				jg.writeStartObject();
				GraphElementDataType dt = vt.getDataType();
				if (dt != null) {
					jg.writeObjectField("labelPath", dt.getDataPath());
					jg.writeObjectField("labelDataType", dt.getTypeName());
				}
				if (vt.getMnemonic() != null) {
					jg.writeObjectField("mnemonic", vt.getMnemonic().toString());
				}
				jg.writeFieldName("visualization");
				jg.writeStartObject();
				SvgVertexVisualizationData svgVisData = (SvgVertexVisualizationData) vt.getVisualizationData();
				SVGDocument svgDoc = svgVisData.getSvgDocument();
				String svgFileName = encodeString(vt.getTypeName()) + ".svg";
				File svgFile = new File(resourceDirectory, svgFileName);
				writeXMLResource(svgDoc.getDocument(), svgFile);
				if (resDirIsTheoryParent) {
					jg.writeObjectField("node", svgFileName);
				} else {
					jg.writeObjectField("node", svgFile.toURI());
				}
				if (svgVisData.getLabelColour() != null) {
					jg.writeFieldName("label");
					jg.writeStartObject();
					Color fill = svgVisData.getLabelColour();
					jg.writeObjectField("fill", colorToString(fill));
					jg.writeEndObject(); // label
				}
				jg.writeEndObject(); // visualization
				jg.writeEndObject();
			}
			jg.writeEndObject(); // vertexTypes
			jg.writeEndObject(); // root
		} finally {
			jg.close();
		}
	}

	private String encodeString(String str) {
		try {
			return URLEncoder.encode(str, "US-ASCII");
		} catch (UnsupportedEncodingException ex) {
			throw new Error(ex);
		}
	}

	private String toTwoDigitHexString(int i) {
		String str = Integer.toHexString(i);
		if (str.length() == 1) {
			return "0" + str;
		}
		return str;
	}

	private String colorToString(Color color) {
		StringBuilder str = new StringBuilder(7);
		str.append('#');
		str.append(toTwoDigitHexString(color.getRed()));
		str.append(toTwoDigitHexString(color.getGreen()));
		str.append(toTwoDigitHexString(color.getBlue()));
		return str.toString();
	}
}
