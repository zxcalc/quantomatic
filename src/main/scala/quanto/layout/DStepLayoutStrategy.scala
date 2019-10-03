package quanto.layout

import quanto.data.{DStep, Graph}

/// Builds GraphLayoutData objects containing the computed layouts for a derivation step.
abstract class DStepLayoutStrategy {
  /// Secondary constructor, which allows the DStep to be provided at construction.
  def this(step : DStep, sourceGraph : Graph) {
    this();
    this.setStep(step, sourceGraph)
  }

  /// Provide a DStep for which the layout can be computed with DStepLayoutStrategy.layout().
  /// @param step the DStep that is to be laid out
  /// @param sourceGraph the source graph of the derivation step
  def setStep(step : DStep, sourceGraph : Graph) : Unit;

  /// Implementations may provide additional methods for setting parameters for the layout algorithm.
  /// Implementations may also provide additional constructors for setting parameters for the layout algorithm.

  /// Create a GraphLayoutData object containing the layout data for the output of the DStep.
  /// The DStep and source graph must have been previously provided to the DStepLayoutStrategy (by constructing with the data or by calling DStepLayoutStrategy.setStep).
  /// @return a GraphLayoutData containing the layout data for the output of the DStep.
  def layoutOutput() : GraphLayoutData;

  /// Create a GraphLayoutData object containing the layout data for the source graph.
  /// The DStep and source graph must have been previously provided to the DStepLayoutStrategy (by constructing with the data or by calling DStepLayoutStrategy.setStep).
  /// @return a GraphLayoutData containing the layout data for the source graph.
  def layoutSource() : GraphLayoutData;
}
