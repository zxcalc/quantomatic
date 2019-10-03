package quanto.layout

import quanto.data.Rule

/// Builds GraphLayoutData objects containing the computed layouts for a rule.
abstract class RuleLayoutStrategy {
  /// Secondary constructor, which allows the Rule to be provided at construction.
  def this(rule : Rule) {
    this();
    this.setRule(rule)
  }

  /// Provide a Rule for which the layout can be computed with RuleLayoutStrategy.layout().
  /// @param rule the Rule that is to be laid out
  def setRule(rule: Rule) : Unit;

  /// Implementations may provide additional methods for setting parameters for the layout algorithm.
  /// Implementations may also provide additional constructors for setting parameters for the layout algorithm.

  /// Create a GraphLayoutData object containing the layout data for the Rule's RHS graph.
  /// The Rule must have been previously provided to the RuleLayoutStrategy (by constructing with the rule or by calling RuleLayoutStrategy.setRule).
  /// @return a GraphLayoutData containing the layout data for the output of the Rule.
  def layoutRHS() : GraphLayoutData;

  /// Create a GraphLayoutData object containing the layout data for the Rule's LHS graph.
  /// The Rule must have been previously provided to the RuleLayoutStrategy (by constructing with the rule or by calling RuleLayoutStrategy.setRule).
  /// @return a GraphLayoutData containing the layout data for the source graph.
  def layoutLHS() : GraphLayoutData;
}
