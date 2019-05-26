package quanto.layout

import quanto.data.{DStep, Graph}

/// Builds GraphLayoutData objects containing the computed layouts for a derivation step.
/// Lifts the GraphLayoutStrategy to a DerivationLayoutStrategy.
class LiftedGraphLayoutDStepLayoutStrategy extends DStepLayoutStrategy {
  private[this] var strategy : GraphLayoutStrategy = null;
  private[this] var step : DStep = null;
  private[this] var sourceGraph : Graph = null;

  /// Secondary constructor, which allows the GraphLayoutStrategy to be provided at construction.
  def this(strategy : GraphLayoutStrategy) {
    this();
    this.setStrategy(strategy);
  }

  /// Secondary constructor, which allows the GraphLayoutStrategy and Derivation to be provided at construction.
  def this(strategy : GraphLayoutStrategy, step : DStep, sourceGraph : Graph) {
    //this(step, sourceGraph); // Secondary constructors cannot call superclass constructors
    this();
    this.setStep(step, sourceGraph);
    this.setStrategy(strategy);
  }

  /// Provide a GraphLayoutStrategy to lift when computing LiftedGraphLayoutDStepLayoutStrategy.layout().
  /// @param strategy the GraphLayoutStrategy to lift
  def setStrategy(strategy : GraphLayoutStrategy) : Unit = {
    this.strategy = strategy;
  }

  /// Provide a DStep for which the layout can be computed with DStepLayoutStrategy.layout().
  /// @param step the DStep that is to be laid out
  /// @param sourceGraph the source graph of the derivation step
  def setStep(step : DStep, sourceGraph : Graph) : Unit = {
    this.step = step;
    this.sourceGraph = sourceGraph;
  }

  /// Create a GraphLayoutData object containing the layout data for the output of the DStep.
  /// The DStep and source graph must have been previously provided to the DStepLayoutStrategy (by constructing with the data or by calling DStepLayoutStrategy.setStep).
  /// @return a GraphLayoutData containing the layout data for the output of the DStep.
  def layoutOutput() : GraphLayoutData = {
    strategy.setGraph(this.step.graph);
    return strategy.layout()
  }

  /// Create a GraphLayoutData object containing the layout data for the source graph.
  /// The DStep and source graph must have been previously provided to the DStepLayoutStrategy (by constructing with the data or by calling DStepLayoutStrategy.setStep).
  /// @return a GraphLayoutData containing the layout data for the source graph.
  def layoutSource() : GraphLayoutData = {
    strategy.setGraph(this.sourceGraph);
    return strategy.layout()
  }
}
