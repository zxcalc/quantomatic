/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.xml;

import java.util.LinkedList;
import java.util.List;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import quanto.core.Theory;
import quanto.core.data.AttachedRewrite;
import quanto.core.data.CoreGraph;

/**
 *
 * @author alex
 */
public class AttachedRewriteListFragmentHandler
extends DefaultFragmentHandler<List<AttachedRewrite<CoreGraph>>> {
	private enum Mode {
		None,
		Rewrites,
		Rewrite
	}
	private Mode mode = Mode.None;
	private List<AttachedRewrite<CoreGraph>> rewrites = null;
	private int unknownElementDepth = 0;
	private Theory theory;
	private CoreGraph graph;
	private int index = -1;
	private AttachedRewriteFragmentHandler currentRewrite = null;

	public AttachedRewriteListFragmentHandler(Theory theory, CoreGraph graph) {
		this.theory = theory;
		this.graph = graph;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		if (unknownElementDepth > 0) {
			++unknownElementDepth;
		} else if (mode == Mode.None) {
			if (!"rewrites".equals(localName))
				throw new SAXException("Start of rewrites fragment expected");
			rewrites = new LinkedList<AttachedRewrite<CoreGraph>>();
			mode = Mode.Rewrites;
		} else if (mode == Mode.Rewrites) {
			if ("rewrite".equals(localName)) {
				++index;
				currentRewrite = new AttachedRewriteFragmentHandler(theory, graph, index);
				currentRewrite.setDocumentLocator(locator);
				currentRewrite.startElement(uri, localName, qName, attributes);
				mode = Mode.Rewrite;
			} else {
				++unknownElementDepth;
			}
		} else if (mode == Mode.Rewrite) {
			currentRewrite.startElement(uri, localName, qName, attributes);
		} else {
			++unknownElementDepth;
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {
		if (unknownElementDepth > 0) {
			--unknownElementDepth;
		} else if (mode == Mode.Rewrites && "rewrites".equals(localName)) {
			mode = Mode.None;
		} else if (mode == Mode.Rewrite) {
			currentRewrite.endElement(uri, localName, qName);
			if (currentRewrite.isComplete()) {
				rewrites.add(currentRewrite.buildResult());
				currentRewrite = null;
				mode = Mode.Rewrites;
			}
		} else {
			throw new IllegalStateException("endElement cannot be called without a corresponding startElement; element was " + localName);
		}
	}

	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		if (mode == Mode.Rewrite)
			currentRewrite.characters(ch, start, length);
	}

	public boolean isComplete() {
		return mode == Mode.None;
	}

	public List<AttachedRewrite<CoreGraph>> buildResult() throws SAXException {
		return rewrites;
	}
	
}
