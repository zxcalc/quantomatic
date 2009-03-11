package quanto.gui;


import java.util.ArrayList;
import java.util.List;
import net.n3.nanoxml.*;

import edu.uci.ics.jung.contrib.HasName;

public class Rewrite implements HasName {
	private QuantoGraph lhs = null;
	private QuantoGraph rhs = null;
	private String name;
	public Rewrite(String name, QuantoGraph lhs, QuantoGraph rhs) {
		this.name = name;
		this.lhs = lhs;
		this.rhs = rhs;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	
	public static List<Rewrite> parseRewrites(String xml) {
		List<Rewrite> rewrites = new ArrayList<Rewrite>();
		try {
			IXMLParser parser = XMLParserFactory.createDefaultXMLParser(new StdXMLBuilder());
			parser.setReader(StdXMLReader.stringReader(xml));
			IXMLElement root = (IXMLElement)parser.parse();
			for (Object obj : root.getChildrenNamed("rewrite")) {
				IXMLElement rw = (IXMLElement)obj;
				IXMLElement ruleName = rw.getFirstChildNamed("rulename");
				if (ruleName == null)
					throw new XMLException("<rewrite> must have a <rulename> element");
				IXMLElement lhs = rw.getFirstChildNamed("lhs")
					.getFirstChildNamed("graph");
				IXMLElement rhs = rw.getFirstChildNamed("rhs")
					.getFirstChildNamed("graph");
				rewrites.add(new Rewrite(
						ruleName.getContent(),
						new QuantoGraph().fromXml(lhs),
						new QuantoGraph().fromXml(rhs)
					));
				
			}
		} catch (XMLException e) {
			System.out.println("Error parsing XML: " + e.getMessage());
		} catch (ClassNotFoundException e) {
			throw new RuntimeException(e);
		} catch (InstantiationException e) {
			throw new RuntimeException(e);
		} catch (IllegalAccessException e) {
			throw new RuntimeException(e);
		}
		return rewrites;
	}
	public QuantoGraph getLhs() {
		return lhs;
	}
	public QuantoGraph getRhs() {
		return rhs;
	}
}
