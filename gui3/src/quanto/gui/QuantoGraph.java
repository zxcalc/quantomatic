package quanto.gui;


import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import net.n3.nanoxml.*;
import edu.uci.ics.jung.contrib.HasName;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.util.EdgeType;

public class QuantoGraph extends SparseMultigraph<QVertex, QEdge>
implements HasName {
	private static final long serialVersionUID = -1519901566511300787L;
	protected String name;
	protected List<QVertex> boundaryVertices;
	protected List<BangBox> bangBoxes;
	private String fileName = null; // defined if this graph is backed by a file
	private boolean saved = true; // true if this graph has been modified since last saved

	public QuantoGraph(String name) {
		this.name = name;
		this.bangBoxes = new ArrayList<BangBox>();
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

	/**
	 * Parse XML from a string. If the core gives mal-formed XML,
	 * the front-end SHOULD crash, so throw QuantoCore.FatalError.
	 * @param xml
	 * @return
	 */
	public QuantoGraph fromXml(String xml) {
		try {
			IXMLParser parser = XMLParserFactory.createDefaultXMLParser(new StdXMLBuilder());
			parser.setReader(StdXMLReader.stringReader(xml));
			IXMLElement root = (IXMLElement)parser.parse();
			fromXml(root);
		} catch (XMLException e) {
			throw new QuantoCore.FatalError("Error parsing XML.");
		} catch (ClassNotFoundException e) {
			throw new QuantoCore.FatalError(e);
		} catch (InstantiationException e) {
			throw new QuantoCore.FatalError(e);
		} catch (IllegalAccessException e) {
			throw new QuantoCore.FatalError(e);
		}
		this.saved = false; // we have changed the graph so it needs to be saved
							// note that if this needs to be TRUE it will be set elsewhere
		return this;
	}

	/**
	 * Populate this graph using a given DOM node. This is in
	 * a separate method so graph defs can be nested inside of
	 * bigger XML blocks, e.g. rewrites.
	 * @param graphNode
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public QuantoGraph fromXml(IXMLElement graphNode) {
		if (graphNode == null)
			throw new QuantoCore.FatalError("Attempting to parse null graph.");
		
		synchronized (this) {
			boundaryVertices = new ArrayList<QVertex>(); 
			for (QEdge e : new ArrayList<QEdge>(getEdges()))
				removeEdge(e);
			
			Map<String, QVertex> verts = getVertexMap();

			for (Object obj : graphNode.getChildrenNamed("vertex")) {
				IXMLElement vertexNode = (IXMLElement)obj;
				QVertex v = new QVertex();
				
				try {
					v.setName(vertexNode.getFirstChildNamed("name").getContent());
					if (vertexNode.getFirstChildNamed("boundary")
							.getContent().equals("true"))
					{
						v.setVertexType(QVertex.Type.BOUNDARY);
					} else {
						v.setVertexType(vertexNode.getFirstChildNamed("colour").getContent());
					}
					
					IXMLElement expr = vertexNode
						.getFirstChildNamed("angleexpr");
					if (expr == null) {
						v.setAngle("0");
					} else {
						v.setAngle(expr.getFirstChildNamed("as_string").getContent());
					}
				} catch (NullPointerException e) {
					/* if NullPointerException is thrown, the
					 * core has most likely neglected to include
					 * a required field, so the GUI should crash.
					 */
					e.printStackTrace();
					throw new QuantoCore.FatalError(
							"Error reading graph XML.");
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
			
			// Prune removed vertices
			for (QVertex v : verts.values()) {
				if (v.old) removeVertex(v);
			}


			for (Object obj : graphNode.getChildrenNamed("edge")) {
				IXMLElement edgeNode = (IXMLElement)obj;

				QVertex source = null, target = null;
				String ename = null;
				IXMLElement ch = null;
				
				ch = edgeNode.getFirstChildNamed("source");
				if (ch!=null) source = verts.get(ch.getContent());
				ch = edgeNode.getFirstChildNamed("target");
				if (ch!=null) target = verts.get(ch.getContent());
				ch = edgeNode.getFirstChildNamed("name");
				if (ch!=null) ename = ch.getContent();

				if (source == null || target == null || ename == null)
					throw new QuantoCore.FatalError(
							"Bad edge definition in XML.");
				
				this.addEdge(new QEdge(ename),
					source, target, EdgeType.DIRECTED);
				
			} // foreach edge
			
			bangBoxes = new ArrayList<BangBox>();
			
			for (IXMLElement bangBox :
				(Vector<IXMLElement>)graphNode.getChildrenNamed("bangbox"))
			{
				IXMLElement nm = bangBox.getFirstChildNamed("name");
				if (nm == null)
					throw new QuantoCore.FatalError("Got an unnamed bang box in XML.");
				
				String name = nm.getContent();
				BangBox bbox = new BangBox(name);
				bangBoxes.add(bbox);
				
				for (IXMLElement boxedVert :
					(Vector<IXMLElement>)bangBox.getChildrenNamed("boxedvertex"))
				{
					QVertex v = verts.get(boxedVert.getContent());
					if (v == null)
						throw new QuantoCore.FatalError("Unknown vertex in bang box");
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
	
	public int getBoundaryIndex(QVertex bv) {
		return boundaryVertices.indexOf(bv);
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

	public void setSaved(boolean unSaved) {
		this.saved = unSaved;
	}

	public List<BangBox> getBangBoxes() {
		return bangBoxes;
	}
}
