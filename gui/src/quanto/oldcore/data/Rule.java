/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.oldcore.data;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import quanto.oldcore.ParseException;
import quanto.oldcore.Theory;

/**
 *
 * @author alemer
 */
public class Rule implements CoreObject {
	private String name;
	private CoreGraph lhs;
	private CoreGraph rhs;
	private Map<String, String> userData = new HashMap<String, String>();

	public Rule(String name, CoreGraph lhs, CoreGraph rhs) {
		this.name = name;
		this.lhs = lhs;
		this.rhs = rhs;
	}

	public Rule() {
	}

	public String getCoreName() {
		return name;
	}

	public void updateCoreName(String name) {
		this.name = name;
	}

	public CoreGraph getLhs() {
		return lhs;
	}

	public CoreGraph getRhs() {
		return rhs;
	}

	public Map<String, String> getUserData() {
		return Collections.unmodifiableMap(userData);
	}
	
	public void setUserData(Map<String, String> map) {
		userData = new HashMap<String, String>(map);
	}
	
	public String getUserDataEntry(String k) {
		return userData.get(k);
	}
	
	public void setUserDataEntry(String k, String v) {
		userData.put(k, v);
	}
	
	public void updateFromJson(Theory theory, JsonNode node) throws ParseException {
		if (!node.isObject())
			throw new ParseException("Expected object");

		JsonNode nameNode = node.get("name");
		if (nameNode != null && nameNode.isTextual())
			updateCoreName(nameNode.asText());
		
		JsonNode lhsNode = node.get("lhs");
		if (lhsNode == null || lhsNode.isNull())
			throw new ParseException("No lhs given for rule");
		if (lhs != null) {
			lhs.updateFromJson(lhsNode);
		} else {
			lhs = CoreGraph.fromJson(theory, null, lhsNode);
		}
		
		JsonNode rhsNode = node.get("rhs");
		if (rhsNode == null || rhsNode.isNull())
			throw new ParseException("No rhs given for rule");
		if (rhs != null) {
			rhs.updateFromJson(rhsNode);
		} else {
			rhs = CoreGraph.fromJson(theory, null, rhsNode);
		}

		JsonNode annotationNode = node.get("annotation");
		if (annotationNode != null && annotationNode.isObject()) {
			ObjectMapper mapper = new ObjectMapper();
			setUserData(mapper.<HashMap<String,String>>convertValue(
					annotationNode,
					mapper.getTypeFactory().constructMapType(
					HashMap.class, String.class, String.class)));
		}
	}
	
	public static Rule fromJson(Theory theory, JsonNode node) throws ParseException {
		if (!node.isObject())
			throw new ParseException("Expected object");

		JsonNode nameNode = node.get("name");
		if (nameNode == null || !nameNode.isTextual())
			throw new ParseException("Standalone rule had no name");

		Rule rule = new Rule();
		rule.updateFromJson(theory, node);
		return rule;
	}

	static Rule fromJson(Theory theory, String name, JsonNode desc) throws ParseException {
		Rule rule = new Rule();
		rule.name = name;
		rule.updateFromJson(theory, desc);
		return rule;
	}
}
