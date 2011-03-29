package quanto.core;


import edu.uci.ics.jung.contrib.DirectedSparseBangBoxMultigraph;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import net.n3.nanoxml.*;
import edu.uci.ics.jung.graph.util.EdgeType;
import edu.uci.ics.jung.visualization.util.ChangeEventSupport;
import java.util.LinkedList;
import java.util.Vector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RGGraph extends DirectedSparseBangBoxMultigraph<RGVertex, BasicEdge, BasicBangBox>
implements CoreGraph<RGVertex, BasicEdge, BasicBangBox>, ChangeEventSupport {

	private final static Logger logger =
		LoggerFactory.getLogger(RGGraph.class);

	private static final long serialVersionUID = -1519901566511300787L;
	protected String name;
	protected Set<ChangeListener> changeListeners;
	
	private String fileName = null; // defined if this graph is backed by a file
	private boolean saved = true; // true if this graph has been modified since last saved

	public RGGraph(String name) {
		this.name = name;
		this.changeListeners = Collections.synchronizedSet(
				new HashSet<ChangeListener>());
	}

	/**
	 * Use this constructor for unnamed graphs. The idea is you
	 * should do null checks before sending the name to the core.
	 */
	public RGGraph() {
		this(null);
	}

	public Map<String,RGVertex> getVertexMap() {
		Map<String, RGVertex> verts =
			new HashMap<String, RGVertex>();
		for (RGVertex v : getVertices()) {
			v.old = true;
			verts.put(v.getCoreName(), v);
		}
		return verts;
	}

	public IXMLElement fromXml(String xml) throws ParseException {
		return fromXmlReader(StdXMLReader.stringReader(xml));
	}
	
	public IXMLElement fromXml(File f) throws java.io.IOException, ParseException {
		return fromXmlReader(StdXMLReader.fileReader(f.getAbsolutePath()));
	}
	
	private IXMLElement fromXmlReader(IXMLReader reader) throws ParseException {
		IXMLElement root = null;
		try {
			long millis = System.currentTimeMillis();
			IXMLParser parser = XMLParserFactory.createDefaultXMLParser(new StdXMLBuilder());
			parser.setReader(reader);
			root = (IXMLElement)parser.parse();
			fromXml(root);
			logger.debug("XML parse took {} milliseconds", System.currentTimeMillis()-millis);
		} catch (XMLException e) {
			throw new ParseException("The file contains badly-formed XML: " + e.getMessage(), e);
		} catch (ClassNotFoundException e) {
			throw new Error(e);
		} catch (InstantiationException e) {
			throw new Error(e);
		} catch (IllegalAccessException e) {
			throw new Error(e);
		}
		
		// tell all the change listeners I have changed
		fireStateChanged();
		return root;
	}

	private void throwParseException(IXMLElement element, String message) throws ParseException
	{
		String finalmsg = "Bad " + element.getName() + " definition";
		if (element.getLineNr() != IXMLElement.NO_LINE)
			finalmsg += " at line " + element.getLineNr();
		if (message != null)
			finalmsg += ": " + message;

		throw new ParseException(finalmsg);
	}

	/**
	 * Populate this graph using a given DOM node. This is in
	 * a separate method so graph defs can be nested inside of
	 * bigger XML blocks, e.g. rewrites.
	 * @param graphNode
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public RGGraph fromXml(IXMLElement graphNode) throws ParseException {
		if (graphNode == null)
			throw new ParseException("Graph is null");
		
		synchronized (this) {
			List<RGVertex> boundaryVertices = new ArrayList<RGVertex>();
			for (BasicEdge e : new ArrayList<BasicEdge>(getEdges()))
				removeEdge(e);
			for (BasicBangBox e : new ArrayList<BasicBangBox>(getBangBoxes()))
				removeBangBox(e);
			
			Map<String, RGVertex> verts = getVertexMap();

			for (Object obj : graphNode.getChildrenNamed("vertex")) {
				IXMLElement vertexNode = (IXMLElement)obj;
				RGVertex v = new RGVertex();
				
				try {
					String vname = vertexNode.getFirstChildNamed("name").getContent();
					if (vname == null || vname.length() == 0)
						throwParseException(vertexNode, "no name given");
					v.updateCoreName(vname);

					if (vertexNode.getFirstChildNamed("boundary")
							.getContent().equals("true"))
					{
						v.setVertexType(RGVertex.Type.BOUNDARY);
					} else if (vertexNode.getFirstChildNamed("boundary")
							.getContent().equals("false")) {
						v.setVertexType(vertexNode.getFirstChildNamed("colour").getContent());
					} else {
						throwParseException(vertexNode, ": invalid value for \"boundary\"");
					}
					
					IXMLElement expr = vertexNode
						.getFirstChildNamed("angleexpr");
					if (expr == null) {
						v.setLabel("0");
					} else {
						v.setLabel(expr.getFirstChildNamed("as_string").getContent());
					}
				} catch (IllegalArgumentException e) {
					throwParseException(vertexNode, null);
				} catch (NullPointerException e) {
					/* if NullPointerException is thrown, the
					 * core has most likely neglected to include
					 * a required field.
					 */
					throwParseException(vertexNode, null);
				}
				
				RGVertex old_v = verts.get(v.getCoreName());
				if (old_v == null) {
					verts.put(v.getCoreName(), v);
					this.addVertex(v);
				} else {
					old_v.updateTo(v);
					v = old_v;
				}
				
				if (v.getVertexType()==RGVertex.Type.BOUNDARY) {
					boundaryVertices.add(v);
				}
			} // foreach vertex
			
			Collections.sort(boundaryVertices, new CoreObject.NameComparator());

                        for (int i = 0; i < boundaryVertices.size(); ++i) {
                                boundaryVertices.get(i).setLabel(String.valueOf(i));
                        }
			
			// Prune removed vertices
			for (RGVertex v : verts.values()) {
				if (v.old) removeVertex(v);
			}


			for (Object obj : graphNode.getChildrenNamed("edge")) {
				IXMLElement edgeNode = (IXMLElement)obj;

				RGVertex source = null, target = null;
				String ename = null;
				IXMLElement ch = null;
				
				ch = edgeNode.getFirstChildNamed("name");
				if (ch!=null)
					ename = ch.getContent();
				if (ename == null || ename.length() == 0)
					throwParseException(edgeNode, "no name given");


				ch = edgeNode.getFirstChildNamed("source");
				if (ch!=null)
					source = verts.get(ch.getContent());
				else
					throwParseException(edgeNode, "no source given");
				if (source == null)
					throwParseException(edgeNode, "unknown source");


				ch = edgeNode.getFirstChildNamed("target");
				if (ch!=null)
					target = verts.get(ch.getContent());
				else
					throwParseException(edgeNode, "no target given");
				if (target == null)
					throwParseException(edgeNode, "unknown target");

				this.addEdge(new BasicEdge(ename),
					source, target, EdgeType.DIRECTED);
				
			} // foreach edge
			
			for (IXMLElement bangBox :
				(Vector<IXMLElement>)graphNode.getChildrenNamed("bangbox"))
			{
				IXMLElement nm = bangBox.getFirstChildNamed("name");
				if (nm == null)
					throwParseException(bangBox, "no name given");

				String bbname = nm.getContent();
				if (bbname == null || bbname.length() == 0)
					throwParseException(bangBox, "no name given");

				BasicBangBox bbox = new BasicBangBox(bbname);
				List contents = new LinkedList();

				for (IXMLElement boxedVert :
					(Vector<IXMLElement>)bangBox.getChildrenNamed("boxedvertex"))
				{
					RGVertex v = verts.get(boxedVert.getContent());
					if (v == null)
						throwParseException(boxedVert, "unknown vertex");
					contents.add(v);
				}
				addBangBox(bbox, contents);
			}
		} // synchronized(this)
		
		return this;
	}
	
	public List<RGVertex> getSubgraphVertices(RGGraph graph) {
		List<RGVertex> verts = new ArrayList<RGVertex>();
		synchronized (this) {
			Map<String,RGVertex> vmap = getVertexMap();
			for (RGVertex v : graph.getVertices()) {
				if (v.getVertexType() == RGVertex.Type.BOUNDARY)
					continue; // don't highlight boundaries
				// find the vertex corresponding to the selected
				//  subgraph, by name
				RGVertex real_v = vmap.get(v.getCoreName());
				if (real_v != null) verts.add(real_v);
			}
		}
		return verts;
	}

	public String getCoreName() {
		return name;
	}

	public void updateCoreName(String name) {
		this.name = name;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public boolean isSaved() {
		return saved;
	}

	public void setSaved(boolean saved) {
		this.saved = saved;
	}

	public void addChangeListener(ChangeListener l) {
		changeListeners.add(l);
	}

	public void fireStateChanged() {
		this.saved = false; // we have changed the graph so it needs to be saved
							// note that if this needs to be TRUE it will be set elsewhere
		synchronized (changeListeners) {
			ChangeEvent evt = new ChangeEvent(this);
			for (ChangeListener l : changeListeners) {
				l.stateChanged(evt);
			}
		}
	}

	public ChangeListener[] getChangeListeners() {
		return changeListeners.toArray(new ChangeListener[changeListeners.size()]);
	}

	public void removeChangeListener(ChangeListener l) {
		changeListeners.remove(l);
	}
}
