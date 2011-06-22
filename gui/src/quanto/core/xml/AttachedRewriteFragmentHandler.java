package quanto.core.xml;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import quanto.core.Theory;
import quanto.core.data.AttachedRewrite;
import quanto.core.data.CoreGraph;

public class AttachedRewriteFragmentHandler
extends DefaultFragmentHandler<AttachedRewrite<CoreGraph>> {
	private enum Mode {
		None,
		Rewrite,
		Rulename,
		Lhs,
		LhsGraph,
		Rhs,
		RhsGraph
	}
	private Mode mode = Mode.None;
	private StringBuilder ruleName = null;
	private Theory theory;
	private CoreGraph graph;
	private int index;
	private GraphFragmentHandler lhs = null;
	private GraphFragmentHandler rhs = null;
	private int unknownElementDepth = 0;

	public AttachedRewriteFragmentHandler(Theory theory, CoreGraph graph, int index) {
		this.theory = theory;
		this.graph = graph;
		this.index = index;
	}

	public boolean isComplete() {
		return mode == Mode.None;
	}

	public AttachedRewrite<CoreGraph> buildResult() throws SAXException {
		return new AttachedRewrite<CoreGraph>(
			graph,
			index,
			ruleName.toString().trim(),
			lhs.buildResult(),
			rhs.buildResult());
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		if (unknownElementDepth > 0) {
			++unknownElementDepth;
		} else if (mode == Mode.None) {
			if (!"rewrite".equals(localName))
				throw new SAXException("Start of rewrite fragment expected");
			mode = Mode.Rewrite;
		} else if (mode == Mode.Rewrite) {
			if ("rulename".equals(localName))
				mode = Mode.Rulename;
			else if ("lhs".equals(localName)) {
				mode = Mode.Lhs;
			} else if ("rhs".equals(localName)) {
				mode = Mode.Rhs;
			} else {
				++unknownElementDepth;
			}
		} else if (mode == Mode.Lhs && "graph".equals(localName)) {
			mode = Mode.LhsGraph;
			lhs = new GraphFragmentHandler(theory, new CoreGraph());
			lhs.setDocumentLocator(locator);
			lhs.startElement(uri, localName, qName, attributes);
		} else if (mode == Mode.Rhs && "graph".equals(localName)) {
			mode = Mode.RhsGraph;
			rhs = new GraphFragmentHandler(theory, new CoreGraph());
			rhs.setDocumentLocator(locator);
			rhs.startElement(uri, localName, qName, attributes);
		} else if (mode == Mode.LhsGraph) {
			lhs.startElement(uri, localName, qName, attributes);
		} else if (mode == Mode.RhsGraph) {
			rhs.startElement(uri, localName, qName, attributes);
		} else {
			++unknownElementDepth;
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {
		if (unknownElementDepth > 0) {
			--unknownElementDepth;
		} else if (mode == Mode.Rewrite && "rewrite".equals(localName)) {
		} else if (mode == Mode.Rulename && "rulename".equals(localName)) {
			mode = Mode.Rewrite;
			if (ruleName.toString().trim().length() == 0)
				throw new SAXParseException("rulename cannot be empty", locator);
		} else if (mode == Mode.Lhs && "lhs".equals(localName)) {
			mode = Mode.Rewrite;
			if (!lhs.isComplete())
				throw new SAXParseException(
					"<lhs> must contain a <graph> element", locator);
		} else if (mode == Mode.Rhs && "rhs".equals(localName)) {
			mode = Mode.Rewrite;
			if (!rhs.isComplete())
				throw new SAXParseException(
					"<rhs> must contain a <graph> element", locator);
		} else if (mode == Mode.LhsGraph) {
			lhs.endElement(uri, localName, qName);
			if (lhs.isComplete())
				mode = Mode.Lhs;
		} else if (mode == Mode.RhsGraph) {
			rhs.endElement(uri, localName, qName);
			if (rhs.isComplete())
				mode = Mode.Rhs;
		} else {
			throw new IllegalStateException("endElement cannot be called without a corresponding startElement; element was " + localName);
		}
	}

	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		if (mode == Mode.Rulename)
			ruleName.append(ch, start, length);
	}
}
