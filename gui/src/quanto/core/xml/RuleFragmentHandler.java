/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.xml;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import quanto.core.Theory;
import quanto.core.data.CoreGraph;
import quanto.core.data.Rule;

/**
 *
 * @author alemer
 */
public class RuleFragmentHandler extends DefaultFragmentHandler<Rule<CoreGraph>>
{
	private enum Mode {
		None,
		Rule,
		Name,
		Definition,
		Lhs,
		Rhs
	}
	private Theory theory;
	private Mode mode = Mode.None;
    private String name = null;
	private CoreGraph lhs = null;
	private CoreGraph rhs = null;
    private GraphFragmentHandler graphHandler = null;
	private int unknownElementDepth = 0;

    public RuleFragmentHandler(Theory theory) {
        this.theory = theory;
    }

    public boolean isComplete() {
        return mode == Mode.None;
    }

    public Rule<CoreGraph> buildResult() throws SAXException {
        return new Rule<CoreGraph>(name, lhs, rhs);
    }

    @Override
    public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
        if (unknownElementDepth > 0) {
            ++unknownElementDepth;
            return;
        }

        if (graphHandler != null) {
            graphHandler.startElement(uri, localName, qName, attributes);
            return;
        }

        switch (mode) {
            case None:
                if (!"rule".equals(localName))
                    throw new SAXException("Start of rule fragment expected");
                mode = Mode.Rule;
                break;

            case Rule:
                if ("name".equals(localName)) {
                    if (name != null)
                        throw new SAXParseException( "<rule> cannot contain multiple <name> elements", locator);
                    mode = Mode.Name;
                    charCollector = new StringBuilder();
                }
                else if ("definition".equals(localName)) {
                    if (lhs != null)
                        throw new SAXParseException( "<rule> cannot contain multiple <definition> elements", locator);
                    mode = Mode.Definition;
                }
                else {
                    ++unknownElementDepth;
                }
                break;

            case Definition:
                if ("lhs".equals(localName)) {
                    if (lhs != null)
                        throw new SAXParseException( "<definition> cannot contain multiple <lhs> elements", locator);
                    mode = Mode.Lhs;
                }
                else if ("rhs".equals(localName)) {
                    if (rhs != null)
                        throw new SAXParseException( "<definition> cannot contain multiple <rhs> elements", locator);
                    mode = Mode.Rhs;
                }
                else {
                    ++unknownElementDepth;
                }
                break;

            case Lhs:
            case Rhs:
                if ("graph".equals(localName)) {
                    CoreGraph graph = new CoreGraph();
                    if (mode == Mode.Lhs)
                        lhs = graph;
                    else
                        rhs = graph;
                    graphHandler = new GraphFragmentHandler(theory, graph);
                    graphHandler.setDocumentLocator(locator);
                    graphHandler.startElement(uri, localName, qName, attributes);
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

        if (graphHandler != null) {
            graphHandler.endElement(uri, localName, qName);
            if (graphHandler.isComplete()) {
                graphHandler.buildResult();
                graphHandler = null;
            }
            return;
        }

        switch (mode) {
            case Rule:
                mode = Mode.None;
                if (name == null)
                    throw new SAXParseException( "<rule> must contain a <name> element", locator);
                if (lhs == null)
                    throw new SAXParseException( "<rule> must contain a <definition> element", locator);
                break;

            case Name:
                mode = Mode.Rule;
                name = charCollector.toString().trim();
                charCollector = null;
                if (name.length() == 0)
                    throw new SAXParseException( "<name> cannot be empty", locator);
                break;

            case Lhs:
                mode = Mode.Definition;
                if (lhs == null)
                    throw new SAXParseException( "<lhs> must contain a <graph> element", locator);
                break;

            case Rhs:
                mode = Mode.Definition;
                if (rhs == null)
                    throw new SAXParseException( "<rhs> must contain a <graph> element", locator);
                break;

            case Definition:
                mode = Mode.Rule;
                if (lhs == null)
                    throw new SAXParseException( "<definition> must contain a <lhs> element", locator);
                if (rhs == null)
                    throw new SAXParseException( "<definition> must contain a <rhs> element", locator);
                break;

            default:
                throw new IllegalStateException("endElement cannot be called without a corresponding startElement; element was " + localName);
        }
    }

    @Override
    public void characters(char[] ch, int start, int length) throws SAXException {
        super.characters(ch, start, length);

        if (graphHandler != null) {
            graphHandler.characters(ch, start, length);
        }
    }
}
