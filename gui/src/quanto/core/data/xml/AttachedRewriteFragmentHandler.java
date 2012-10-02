package quanto.core.data.xml;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import quanto.core.Theory;
import quanto.core.data.AttachedRewrite;
import quanto.core.data.CoreGraph;

public class AttachedRewriteFragmentHandler
extends DefaultFragmentHandler<AttachedRewrite> {
	private enum Mode {
		None,
		Rewrite,
		Newgraph
	}
	private Mode mode = Mode.None;
	private Theory theory;
	private CoreGraph graph;
	private int index;
	private RuleFragmentHandler ruleHandler = null;
    private GraphFragmentHandler newGraphHandler = null;
    private FragmentHandler activeHandler = null;
	private int unknownElementDepth = 0;

	public AttachedRewriteFragmentHandler(Theory theory, CoreGraph graph, int index) {
		this.theory = theory;
		this.graph = graph;
		this.index = index;
	}

	public boolean isComplete() {
		return mode == Mode.None;
	}

	public AttachedRewrite buildResult() throws SAXException {
		return new AttachedRewrite(
			graph,
			index,
			ruleHandler.buildResult(),
            newGraphHandler.buildResult());
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		if (unknownElementDepth > 0) {
			++unknownElementDepth;
            return;
		}

        if (activeHandler != null) {
            activeHandler.startElement(uri, localName, qName, attributes);
            return;
        }

        switch (mode) {
            case None:
                if (!"rewrite".equals(localName))
                    throw new SAXException("Start of rewrite fragment expected");
                mode = Mode.Rewrite;

                break;

            case Rewrite:
                if ("rule".equals(localName)) {
                    if (ruleHandler != null)
                        throw new SAXParseException( "<rewrite> cannot contain multiple <rule> elements", locator);
                    ruleHandler = new RuleFragmentHandler(theory);
                    activeHandler = ruleHandler;
                    activeHandler.setDocumentLocator(locator);
                    activeHandler.startElement(uri, localName, qName, attributes);
                }
                else if ("newgraph".equals(localName)) {
                    mode = Mode.Newgraph;
                    if (newGraphHandler != null)
                        throw new SAXParseException( "<rewrite> cannot contain multiple <newgraph> elements", locator);
                }
                else {
                    ++unknownElementDepth;
                }
                break;

            case Newgraph:
                if (newGraphHandler != null && !newGraphHandler.isComplete()) {
                    newGraphHandler.startElement(uri, localName, qName, attributes);
                }
                else if ("graph".equals(localName)) {
                    newGraphHandler = new GraphFragmentHandler(theory, new CoreGraph());
                    activeHandler = newGraphHandler;
                    activeHandler.setDocumentLocator(locator);
                    activeHandler.startElement(uri, localName, qName, attributes);
                }
                else {
                    ++unknownElementDepth;
                }
                break;

            default:
                ++unknownElementDepth;
        }
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {
		if (unknownElementDepth > 0) {
			--unknownElementDepth;
            return;
		}

        if (activeHandler != null) {
            activeHandler.endElement(uri, localName, qName);
            if (activeHandler.isComplete()) {
                activeHandler = null;
            }
            return;
        }

        switch (mode) {
            case Rewrite:
                assert("rewrite".equals(localName));
                mode = Mode.None;
                if (newGraphHandler == null)
                    throw new SAXParseException( "<rewrite> must contain a <newgraph> element", locator);
                if (ruleHandler == null)
                    throw new SAXParseException( "<rewrite> must contain a <rule> element", locator);
                break;

            case Newgraph:
                assert("newgraph".equals(localName));
                mode = Mode.Rewrite;
                if (newGraphHandler == null)
                    throw new SAXParseException( "<newgraph> must contain a <graph> element", locator);
                break;

            default:
                throw new IllegalStateException("endElement cannot be called without a corresponding startElement; element was " + localName);
        }
	}

    @Override
    public void characters(char[] ch, int start, int length) throws SAXException {
        super.characters(ch, start, length);

        if (activeHandler != null)
            activeHandler.characters(ch, start, length);
    }

    @Override
    public void ignorableWhitespace(char[] ch, int start, int length) throws SAXException {
        super.ignorableWhitespace(ch, start, length);

        if (activeHandler != null)
            activeHandler.characters(ch, start, length);
    }
}
