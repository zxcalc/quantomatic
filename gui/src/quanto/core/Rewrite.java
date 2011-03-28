package quanto.core;


import java.util.ArrayList;
import java.util.List;
import net.n3.nanoxml.*;


public class Rewrite implements CoreObject {
	private QGraph lhs = null;
	private QGraph rhs = null;
	private String name;
	public Rewrite(String name, QGraph lhs, QGraph rhs) {
		this.name = name;
		this.lhs = lhs;
		this.rhs = rhs;
	}
	public String getCoreName() {
		return name;
	}
	public void updateCoreName(String name) {
		this.name = name;
	}
	
	public static List<Rewrite> parseRewrites(String xml) throws ParseException {
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
						new QGraph().fromXml(lhs),
						new QGraph().fromXml(rhs)
					));
				
			}
		} catch (XMLException e) {
			throw new ParseException(e.getMessage(), e);
		} catch (ClassNotFoundException e) {
			throw new RuntimeException(e);
		} catch (InstantiationException e) {
			throw new RuntimeException(e);
		} catch (IllegalAccessException e) {
			throw new RuntimeException(e);
		}
		return rewrites;
	}
	public QGraph getLhs() {
		return lhs;
	}
	public QGraph getRhs() {
		return rhs;
	}
}
