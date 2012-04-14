package quanto.core;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.event.EventListenerList;
import org.apache.commons.collections15.CollectionUtils;
import org.apache.commons.collections15.Transformer;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;
import quanto.core.data.*;
import quanto.core.protocol.ProtocolManager;
import quanto.core.xml.EdgeFragmentHandler.EdgeData;
import quanto.core.xml.*;

/**
 * Provides a nicer interface to the core
 * 
 * @author alex
 */
public class Core {

    private final static Logger logger = Logger.getLogger("quanto.core");
    
    EventListenerList listenerList = new EventListenerList();

    private ProtocolManager talker;
    private Theory activeTheory;
    private Ruleset ruleset;

    private <T> T parseXml(String xml, FragmentHandler<? extends T> handler) throws CoreException {
        try {
            InputSource source = new InputSource(new StringReader(xml));
            XMLReader reader = XMLReaderFactory.createXMLReader();
            SAXFragmentAdaptor<T> adaptor = new SAXFragmentAdaptor<T>(handler);
            reader.setContentHandler(adaptor);
            reader.parse(source);
            return adaptor.getResult();
        } catch (SAXParseException ex) {
            logger.log(Level.SEVERE, "Failed to parse from core", ex);
            throw new CoreCommunicationException("Could not parse XML from the core", ex);
        } catch (SAXException ex) {
            logger.log(Level.SEVERE, "Error when parsing XML", ex);
            throw new CoreCommunicationException("Failed to parse XML", ex);
        } catch (IOException ex) {
            // this should never happen!
            logger.log(Level.SEVERE, "Error when reading from a String", ex);
            throw new CoreCommunicationException("Failed to read XML from String", ex);
        }
    }

    public Core() throws CoreException {
        this.talker = new ProtocolManager();
        talker.startCore();
        this.ruleset = new Ruleset(this);
    }

    public Core(Theory theory) throws CoreException {
        this();
        talker.changeTheory(theory.getCoreName());
        this.activeTheory = theory;
    }

    public void updateCoreTheory(Theory theory) throws CoreException {
        fireTheoryAboutToChange(theory);
        Theory oldTheory = activeTheory;
        talker.changeTheory(theory.getCoreName());
        this.activeTheory = theory;
        fireTheoryChanged(oldTheory);
    }

    public void addCoreChangeListener(CoreChangeListener l) {
        listenerList.add(CoreChangeListener.class, l);
    }

    public void removeCoreChangeListener(CoreChangeListener l) {
        listenerList.remove(CoreChangeListener.class, l);
    }

    protected void fireTheoryAboutToChange(Theory newTheory) {
        TheoryChangeEvent coreEvent = null;
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==CoreChangeListener.class) {
                // Lazily create the event:
                if (coreEvent == null)
                    coreEvent = new TheoryChangeEvent(this, activeTheory, newTheory);
                ((CoreChangeListener)listeners[i+1]).theoryAboutToChange(coreEvent);
            }
        }
    }

    protected void fireTheoryChanged(Theory oldTheory) {
        TheoryChangeEvent coreEvent = null;
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==CoreChangeListener.class) {
                // Lazily create the event:
                if (coreEvent == null)
                    coreEvent = new TheoryChangeEvent(this, oldTheory, activeTheory);
                ((CoreChangeListener)listeners[i+1]).theoryChanged(coreEvent);
            }
        }
    }

    public Theory getActiveTheory() {
        return activeTheory;
    }

    public Ruleset getRuleset() {
        return ruleset;
    }
    private Transformer<CoreObject, String> namer = new Transformer<CoreObject, String>() {

        public String transform(CoreObject i) {
            return i.getCoreName();
        }
    };

    private Collection<String> names(Collection<? extends CoreObject> c) {
        if (c == null) {
            return null;
        }
        return CollectionUtils.collect(c, namer);
    }

    public ProtocolManager getTalker() {
        return talker;
    }

    private void assertCoreGraph(CoreGraph graph) {
        if (graph.getCoreName() == null) {
            throw new IllegalStateException("The graph does not have a name");
        }
    }

    public CoreGraph createEmptyGraph() throws CoreException {
        return new CoreGraph(talker.loadEmptyGraph());
    }

    public CoreGraph loadGraph(File location) throws CoreException, IOException {
        CoreGraph g = new CoreGraph(talker.loadGraphFromFile(location.getAbsolutePath()));
        updateGraph(g);
        g.setFileName(location.getAbsolutePath());
        return g;
    }
    
    public void saveGraph(CoreGraph graph, File location) throws CoreException,
            IOException {
        assertCoreGraph(graph);
        talker.saveGraphToFile(graph.getCoreName(), location.getAbsolutePath());
    }

    public void updateGraph(CoreGraph graph) throws CoreException {
        String xml = talker.exportGraphAsXml(graph.getCoreName());
        parseXml(xml, new GraphFragmentHandler(activeTheory, graph));
    }

    public enum RepresentationType {

        Plain, Latex, Mathematica, Matlab
    }

    public String hilbertSpaceRepresentation(CoreGraph graph,
            RepresentationType format) throws CoreException {
        ProtocolManager.GraphExportFormat exportFormat;
        switch (format) {
            case Plain:
                exportFormat = ProtocolManager.GraphExportFormat.HilbertTerm;
                break;
            case Latex:
                exportFormat = ProtocolManager.GraphExportFormat.Tikz;
                break;
            case Mathematica:
                exportFormat = ProtocolManager.GraphExportFormat.Mathematica;
                break;
            case Matlab:
                exportFormat = ProtocolManager.GraphExportFormat.Matlab;
                break;
            default:
                throw new IllegalArgumentException("Invalid format");
        }
        return talker.exportGraph(graph.getCoreName(), exportFormat);
    }

    public void renameGraph(CoreGraph graph, String suggestedNewName)
            throws CoreException {
        assertCoreGraph(graph);
        graph.updateCoreName(talker.renameGraph(graph.getCoreName(),
                suggestedNewName));
    }

    public void forgetGraph(CoreGraph graph) throws CoreException {
        assertCoreGraph(graph);
        talker.discardGraph(graph.getCoreName());
        graph.updateCoreName(null);
    }

    public void undo(CoreGraph graph) throws CoreException {
        assertCoreGraph(graph);
        talker.undo(graph.getCoreName());
    }

    public void redo(CoreGraph graph) throws CoreException {
        assertCoreGraph(graph);
        talker.redo(graph.getCoreName());
    }

    public void undoRewrite(CoreGraph graph) throws CoreException {
         assertCoreGraph(graph);
         talker.undoRewrite(graph.getCoreName());
     }

     public void redoRewrite(CoreGraph graph) throws CoreException {
         assertCoreGraph(graph);
         talker.redoRewrite(graph.getCoreName());
     }
    
    public void startUndoGroup(CoreGraph graph) throws CoreException {
         assertCoreGraph(graph);
         talker.startUndoGroup(graph.getCoreName());
     }

     public void endUndoGroup(CoreGraph graph) throws CoreException {
         assertCoreGraph(graph);
         talker.endUndoGroup(graph.getCoreName());
     }
    
    public Vertex addVertex(CoreGraph graph, VertexType vertexType)
            throws CoreException {
        return addVertex(graph, vertexType.getTypeName());
    }

    public Vertex addVertex(CoreGraph graph, String vertexType)
            throws CoreException {
        assertCoreGraph(graph);
        String xml = talker.addVertex(graph.getCoreName(), vertexType);
        Vertex v = this.<Vertex>parseXml(xml, new VertexFragmentHandler(activeTheory));
        graph.addVertex(v);
        graph.fireStateChanged();    
        return v;
    }

    public Vertex addBoundaryVertex(CoreGraph graph) throws CoreException {
        return addVertex(graph, "edge-point");
    }

    public void setVertexAngle(CoreGraph graph, Vertex v, String angle)
            throws CoreException {
        assertCoreGraph(graph);
        talker.setVertexData(graph.getCoreName(), v.getCoreName(), angle);
        v.getData().setValue(angle);
        graph.fireStateChanged();
    }

    public void deleteVertices(CoreGraph graph, Collection<Vertex> vertices)
            throws CoreException {
        assertCoreGraph(graph);
        talker.deleteVertices(graph.getCoreName(), names(vertices));
        for (Vertex v : vertices) {
            graph.removeVertex(v);
        }
        graph.fireStateChanged();
    }

    public Edge addEdge(CoreGraph graph, boolean directed, Vertex source, Vertex target)
            throws CoreException {
        assertCoreGraph(graph);
        String xml = talker.addEdge(graph.getCoreName(),
                "unit",
                directed,
                source.getCoreName(),
                target.getCoreName());
        EdgeData e = this.<EdgeData>parseXml(xml, new EdgeFragmentHandler());

        if (!source.getCoreName().equals(e.sourceName)) {
            throw new CoreException("Source name from core did not match what we sent");
        }
        if (!target.getCoreName().equals(e.targetName)) {
            throw new CoreException("Target name from core did not match what we sent");
        }
        graph.addEdge(e.edge, source, target);
        graph.fireStateChanged();
        return e.edge;
    }

    public void deleteEdges(CoreGraph graph, Collection<Edge> edges)
            throws CoreException {
        assertCoreGraph(graph);
        talker.deleteEdges(graph.getCoreName(), names(edges));
        for (Edge e : edges) {
            graph.removeEdge(e);
        }
        graph.fireStateChanged();
    }

    public BangBox addBangBox(CoreGraph graph, Collection<Vertex> vertices)
            throws CoreException {
        assertCoreGraph(graph);
        BangBox bb = new BangBox(talker.addBangBox(graph.getCoreName(), names(vertices)));
        graph.addBangBox(bb, vertices);
        graph.fireStateChanged();
        return bb;
    }

    public void bangVertices(CoreGraph graph, String bangBox, Collection<Vertex> vertices)
              throws CoreException {
          assertCoreGraph(graph);
          talker.bangVertices(graph.getCoreName(), bangBox, names(vertices));
          graph.fireStateChanged();
     }
    
    public void removeVerticesFromBangBoxes(CoreGraph graph,
            Collection<Vertex> vertices) throws CoreException {
        assertCoreGraph(graph);
        talker.unbangVertices(graph.getCoreName(), names(vertices));
        updateGraph(graph);
    }

    public void dropBangBoxes(CoreGraph graph, Collection<BangBox> bboxen)
            throws CoreException {
        assertCoreGraph(graph);
        talker.dropBangBoxes(graph.getCoreName(), names(bboxen));
        for (BangBox bb : bboxen) {
            graph.removeBangBox(bb);
        }
        graph.fireStateChanged();
    }

    public void killBangBoxes(CoreGraph graph, Collection<BangBox> bboxen)
            throws CoreException {
        assertCoreGraph(graph);
        talker.killBangBoxes(graph.getCoreName(), names(bboxen));
        for (BangBox bb : bboxen) {
            List<Vertex> verts = new ArrayList<Vertex>(graph.getBoxedVertices(bb));
            for (Vertex v : verts) {
                graph.removeVertex(v);
            }
            graph.removeBangBox(bb);
        }
        graph.fireStateChanged();
    }

    public BangBox mergeBangBoxes(CoreGraph graph, Collection<BangBox> bboxen)
            throws CoreException {
        assertCoreGraph(graph);
        BangBox newbb = new BangBox(talker.mergeBangBoxes(graph.getCoreName(),
                names(bboxen)));
        List<Vertex> contents = new LinkedList<Vertex>();
        for (BangBox bb : bboxen) {
            for (Vertex v : graph.getBoxedVertices(bb)) {
                contents.add(v);
            }
            graph.removeBangBox(bb);
        }
        graph.addBangBox(newbb, contents);
        graph.fireStateChanged();
        return newbb;
    }

    public BangBox duplicateBangBox(CoreGraph graph, BangBox bbox)
            throws CoreException {
        assertCoreGraph(graph);
        String name = talker.duplicateBangBox(graph.getCoreName(),
                bbox.getCoreName());
        updateGraph(graph);
        graph.fireStateChanged();
        for (BangBox bb : graph.getBangBoxes()) {
            if (bb.getCoreName().equals(name)) {
                return bb;
            }
        }
        return null;
    }

    public void replaceRuleset(File location) throws CoreException, IOException {
        talker.replaceRulesetFromFile(location.getAbsolutePath());
        this.ruleset.reload();
    }

    public void replaceRuleset(byte[] ruleset) throws CoreException, IOException {
        talker.replaceRulesetFromData(ruleset);
        this.ruleset.reload();
    }

    public void renameBangBox(CoreGraph graph, String oldName, String newName) 
         throws CoreException {
         assertCoreGraph(graph);
         talker.renameBangBox(graph.getCoreName(), oldName, newName);
    }
    
    public void loadRuleset(File location) throws CoreException, IOException {
        talker.importRulesetFromFile(location.getAbsolutePath());
        this.ruleset.reload();
    }

    public void loadRuleset(byte[] ruleset) throws CoreException, IOException {
        talker.importRulesetFromData(ruleset);
        this.ruleset.reload();
    }

    public void saveRuleset(File location) throws CoreException, IOException {
        talker.exportRulesetToFile(location.getAbsolutePath());
    }

    public byte[] getRulesetEncoded() throws CoreException, IOException {
        return talker.exportRulesetToData();
    }

    /**
     * Creates a rule from two graphs.
     * 
     * Any existing rule with the same name will be replaced.
     *
     * @param ruleName
     * @param lhs
     * @param rhs
     * @return
     * @throws CoreException 
     */
    public Rule<CoreGraph> createRule(String ruleName, CoreGraph lhs,
            CoreGraph rhs) throws CoreException {
        assertCoreGraph(lhs);
        assertCoreGraph(rhs);
        talker.setRule(ruleName, lhs.getCoreName(), rhs.getCoreName());
        // FIXME: get actual rule active state from core
        if (!this.ruleset.getRules().contains(ruleName))
            this.ruleset.ruleAdded(ruleName, false);
        return new Rule<CoreGraph>(ruleName, lhs, rhs);
    }

    public Rule<CoreGraph> openRule(String ruleName) throws CoreException {
        CoreGraph lhs = new CoreGraph(talker.openRuleLhs(ruleName));
        updateGraph(lhs);
        CoreGraph rhs = new CoreGraph(talker.openRuleRhs(ruleName));
        updateGraph(rhs);
        return new Rule<CoreGraph>(ruleName, lhs, rhs);
    }

    public void saveRule(Rule<CoreGraph> rule) throws CoreException {
        if (rule.getCoreName() == null) {
            throw new IllegalArgumentException("Rule has no name");
        }
        talker.setRule(rule.getCoreName(), rule.getLhs().getCoreName(),
                rule.getRhs().getCoreName());
    }

    public void fastNormalise(CoreGraph graph) throws CoreException {
        boolean didRewrites = false;
        try {
            int rwCount = talker.attachOneRewrite(graph.getCoreName());
            while (rwCount > 0) {
                talker.applyAttachedRewrite(graph.getCoreName(), 0);
                didRewrites = true;
                rwCount = talker.attachOneRewrite(graph.getCoreName());
            }
        } catch (CoreException e) {
            if (!e.getMessage().contains("No more rewrites.")) {
                throw e;
            }
        }
        if (didRewrites) {
            updateGraph(graph);
        }
    }

    public void cutSubgraph(CoreGraph graph, Collection<Vertex> vertices)
            throws CoreException {
        assertCoreGraph(graph);
        Collection<String> vnames = names(vertices);
        talker.copySubgraphAndOverwrite(graph.getCoreName(), "__clip__", vnames);
        talker.deleteVertices(graph.getCoreName(), vnames);
        for (Vertex v : vertices) {
            graph.removeVertex(v);
        }
        graph.fireStateChanged();
    }

    public void copySubgraph(CoreGraph graph, Collection<Vertex> vertices)
            throws CoreException {
        assertCoreGraph(graph);
        talker.copySubgraphAndOverwrite(graph.getCoreName(), "__clip__", names(vertices));
    }

    public void paste(CoreGraph target) throws CoreException {
        assertCoreGraph(target);
        talker.insertGraph("__clip__", target.getCoreName());
        updateGraph(target);
    }

    public int attachRewrites(CoreGraph graph, Collection<Vertex> vertices)
            throws CoreException {
        return talker.attachRewrites(graph.getCoreName(), names(vertices));
    }

    public boolean attachOneRewrite(CoreGraph graph, Collection<Vertex> vertices)
            throws CoreException {
        return talker.attachOneRewrite(graph.getCoreName(), names(vertices)) > 0;
    }

    public List<AttachedRewrite<CoreGraph>> getAttachedRewrites(CoreGraph graph)
            throws CoreException {
        String xml = talker.listAttachedRewrites(graph.getCoreName());
        AttachedRewriteListFragmentHandler handler =
                new AttachedRewriteListFragmentHandler(activeTheory, graph);
        return this.<List<AttachedRewrite<CoreGraph>>>parseXml(xml, handler);
    }

    public void applyAttachedRewrite(CoreGraph graph, int i)
            throws CoreException {
        talker.applyAttachedRewrite(graph.getCoreName(), i);
    }

    public String[] renameVertex(CoreGraph graph, String oldName, String newName)
	 					throws CoreException	{
			String[] names = talker.renameVertex(graph.getCoreName(), oldName, newName);
			return names;
	}	
}
