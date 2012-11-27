package quanto.core.data;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import quanto.core.ParseException;
import quanto.core.Theory;


/**
 * A bang box
 *
 * @author alemer
 */
public class BangBox extends GraphElement {
	public BangBox(String name) {
		super(name);
	}

	public BangBox() {
		this(null);
	}
	
	public static class BangBoxData
	{
		BangBox bangBox;
		String parent;
		Collection<String> contents;
	}
	
	public static BangBoxData fromJson(Theory theory, JsonNode node) throws ParseException {
		if (!node.isObject())
			throw new ParseException("Expected object");

		JsonNode nameNode = node.get("name");
		if (nameNode == null || !nameNode.isTextual())
			throw new ParseException("Standalone BangBox had no name");

		BangBox BangBox = new BangBox(nameNode.textValue());
		return BangBox.updateFromJson(theory, node);
	}
	
	BangBoxData updateFromJson(Theory theory, JsonNode node) throws ParseException {
		if (!node.isObject())
			throw new ParseException("Expected object");

		JsonNode nameNode = node.get("name");
		if (nameNode != null && nameNode.isTextual())
			updateCoreName(nameNode.asText());

		JsonNode dataNode = node.get("data");
		// FIXME: BangBox data

		JsonNode annotationNode = node.get("annotation");
		if (annotationNode != null && annotationNode.isObject()) {
			ObjectMapper mapper = new ObjectMapper();
			setUserData(mapper.<HashMap<String,String>>convertValue(
					annotationNode,
					mapper.getTypeFactory().constructMapType(
					HashMap.class, String.class, String.class)));
		}
		
		BangBoxData bbd = new BangBoxData();
		bbd.bangBox = this;

		JsonNode parentNode = node.get("parent");
		if (parentNode != null && !parentNode.isNull()) {
			if (!parentNode.isTextual())
				throw new ParseException("BangBox parent was not a string");
			bbd.parent = parentNode.asText();
		}

		JsonNode contentsNode = node.get("contents");
		if (contentsNode != null && !contentsNode.isNull()) {
			if (!contentsNode.isArray())
				throw new ParseException("BangBox contents was not an array");
			
			ObjectMapper mapper = new ObjectMapper();
			bbd.contents = mapper.convertValue(contentsNode,
					mapper.getTypeFactory().constructCollectionType(
					Collection.class, String.class));
		} else {
			bbd.contents = Collections.emptySet();
		}
		
		return bbd;
	}
	
	static BangBoxData fromJson(Theory theory, String name, JsonNode desc) throws ParseException {
		BangBox BangBox = new BangBox(name);
		return BangBox.updateFromJson(theory, desc);
	}
}
