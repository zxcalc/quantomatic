package quanto.gui;


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
import edu.uci.ics.jung.contrib.HasName;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.util.EdgeType;
import edu.uci.ics.jung.visualization.util.ChangeEventSupport;
import java.util.Vector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class QuantoGraph extends DirectedSparseMultigraph<QVertex, QEdge>
implements HasName, ChangeEventSupport {

	private final static Logger logger =
		LoggerFactory.getLogger(QuantoGraph.class);

	private static final long serialVersionUID = -1519901566511300787L;
	protected String name;
	protected List<QVertex> boundaryVertices;
	protected List<BangBox> bangBoxes;
	protected Set<ChangeListener> changeListeners;
	
	private String fileName = null; // defined if this graph is backed by a file
	private boolean saved = true; // true if this graph has been modified since last saved

	public QuantoGraph(String name) {
		this.name = name;
		this.bangBoxes = new ArrayList<BangBox>();
		this.changeListeners = Collections.synchronizedSet(
				new HashSet<ChangeListener>());
	}

	public static class ParseException extends Exception {
		private static final long serialVersionUID = 2342374892173482937L;
		public ParseException() { }
		public ParseException(String message) {
			super(message);
		}
		public ParseException(Throwable cause) {
			super(cause);
		}
		public ParseException(String message, Throwable cause) {
			super(message, cause);
		}
	}

	/**
	 * Use this constructor for unnamed graphs. The idea is you
	 * should do null checks before sending the name to the core.
	 */
	public QuantoGraph() {
		this(null);
	}

	public Map<String,QVertex> getVertexMap() {
		Map<String, QVertex> verts =
			new HashMap<String, QVertex>();
		for (QVertex v : getVertices()) {
			v.old = true;
			verts.put(v.getName(), v);
		}
		return verts;
	}

	public IXMLElement fromXml(String xml) throws QuantoGraph.ParseException {
		return fromXmlReader(StdXMLReader.stringReader(xml));
	}
	
	public IXMLElement fromXml(File f) throws java.io.IOException, QuantoGraph.ParseException {
		return fromXmlReader(StdXMLReader.fileReader(f.getAbsolutePath()));
	}
	
	private IXMLElement fromXmlReader(IXMLReader reader) throws QuantoGraph.ParseException {
		IXMLElement root = null;
		try {
			long millis = System.currentTimeMillis();
			IXMLParser parser = XMLParserFactory.createDefaultXMLParser(new StdXMLBuilder());
			parser.setReader(reader);
			root = (IXMLElement)parser.parse();
			fromXml(root);
			logger.debug("XML parse took {} milliseconds", System.currentTimeMillis()-millis);
		} catch (XMLException e) {
			throw new QuantoGraph.ParseException("The file contains badly-formed XML: " + e.getMessage(), e);
		} catch (ClassNotFoundException e) {
			throw new QuantoCore.FatalError(e);
		} catch (InstantiationException e) {
			throw new QuantoCore.FatalError(e);
		} catch (IllegalAccessException e) {
			throw new QuantoCore.FatalError(e);
		}
		
		// tell all the change listeners I have changed
		fireStateChanged();
		return root;
	}

	private void throwParseException(IXMLElement element, String message) throws QuantoGraph.ParseException
	{
		String finalmsg = "Bad " + element.getName() + " definition";
		if (element.getLineNr() != IXMLElement.NO_LINE)
			finalmsg += " at line " + element.getLineNr();
		if (message != null)
			finalmsg += ": " + message;

		throw new QuantoGraph.ParseException(finalmsg);
	}

	/**
	 * Populate this graph using a given DOM node. This is in
	 * a separate method so graph defs can be nested inside of
	 * bigger XML blocks, e.g. rewrites.
	 * @param graphNode
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public QuantoGraph fromXml(IXMLElement graphNode) throws QuantoGraph.ParseException {
		if (graphNode == null)
			throw new QuantoGraph.ParseException("Graph is null");
		
		synchronized (this) {
			boundaryVertices = new ArrayList<QVertex>(); 
			for (QEdge e : new ArrayList<QEdge>(getEdges()))
				removeEdge(e);
			
			Map<String, QVertex> verts = getVertexMap();

			for (Object obj : graphNode.getChildrenNamed("vertex")) {
				IXMLElement vertexNode = (IXMLElement)obj;
				QVertex v = new QVertex();
				
				try {
					String vname = vertexNode.getFirstChildNamed("name").getContent();
					if (vname == null || vname.length() == 0)
						throwParseException(vertexNode, "no name given");
					v.setName(vname);

					if (vertexNode.getFirstChildNamed("boundary")
							.getContent().equals("true"))
					{
						v.setVertexType(QVertex.Type.BOUNDARY);
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
				
				QVertex old_v = verts.get(v.getName());
				if (old_v == null) {
					verts.put(v.getName(), v);
					this.addVertex(v);
				} else {
					old_v.updateTo(v);
					v = old_v;
				}
				
				if (v.getVertexType()==QVertex.Type.BOUNDARY) {
					boundaryVertices.add(v);
				}
			} // foreach vertex
			
			Collections.sort(boundaryVertices, new HasName.NameComparator());

                        for (int i = 0; i < boundaryVertices.size(); ++i) {
                                boundaryVertices.get(i).setLabel(String.valueOf(i));
                        }
			
			// Prune removed vertices
			for (QVertex v : verts.values()) {
				if (v.old) removeVertex(v);
			}


			for (Object obj : graphNode.getChildrenNamed("edge")) {
				IXMLElement edgeNode = (IXMLElement)obj;

				QVertex source = null, target = null;
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

				this.addEdge(new QEdge(ename),
					source, target, EdgeType.DIRECTED);
				
			} // foreach edge
			
			bangBoxes = new ArrayList<BangBox>();
			
			for (IXMLElement bangBox :
				(Vector<IXMLElement>)graphNode.getChildrenNamed("bangbox"))
			{
				IXMLElement nm = bangBox.getFirstChildNamed("name");
				if (nm == null)
					throwParseException(bangBox, "no name given");

				String bbname = nm.getContent();
				if (bbname == null || bbname.length() == 0)
					throwParseException(bangBox, "no name given");

				BangBox bbox = new BangBox(bbname);
				bangBoxes.add(bbox);

				for (IXMLElement boxedVert :
					(Vector<IXMLElement>)bangBox.getChildrenNamed("boxedvertex"))
				{
					QVertex v = verts.get(boxedVert.getContent());
					if (v == null)
						throwParseException(boxedVert, "unknown vertex");
					bbox.add(v);
				}
			}
		} // synchronized(this)
		
		return this;
	}
	
	public List<QVertex> getSubgraphVertices(QuantoGraph graph) {
		List<QVertex> verts = new ArrayList<QVertex>();
		synchronized (this) {
			Map<String,QVertex> vmap = getVertexMap();
			for (QVertex v : graph.getVertices()) {
				if (v.getVertexType() == QVertex.Type.BOUNDARY)
					continue; // don't highlight boundaries
				// find the vertex corresponding to the selected
				//  subgraph, by name
				QVertex real_v = vmap.get(v.getName());
				if (real_v != null) verts.add(real_v);
			}
		}
		return verts;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
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

	public List<BangBox> getBangBoxes() {
		return bangBoxes;
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
