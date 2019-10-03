package quanto.layout

import quanto.data.Rule

/// Builds GraphLayoutData objects containing the computed layouts for a derivation step.
/// Lifts the GraphLayoutStrategy to a DerivationLayoutStrategy.
class LiftedGraphLayoutRuleLayoutStrategy extends RuleLayoutStrategy {
  private[this] var strategy : GraphLayoutStrategy = null;
  private[this] var rule : Rule = null;

  /// Secondary constructor, which allows the GraphLayoutStrategy to be provided at construction.
  def this(strategy : GraphLayoutStrategy) {
    this();
    this.setStrategy(strategy);
  }

  /// Secondary constructor, which allows the GraphLayoutStrategy and Derivation to be provided at construction.
  def this(strategy : GraphLayoutStrategy, rule : Rule) {
    //this(step, sourceGraph); // Secondary constructors cannot call superclass constructors
    this();
    this.setRule(rule);
    this.setStrategy(strategy);
  }

  /// Provide a GraphLayoutStrategy to lift when computing LiftedGraphLayoutRuleLayoutStrategy.layout().
  /// @param strategy the GraphLayoutStrategy to lift
  def setStrategy(strategy : GraphLayoutStrategy) : Unit = {
    this.strategy = strategy;
  }

  /// Provide a Rule for which the layout can be computed with LiftedGraphLayoutRuleLayoutStrategy.layout().
  /// @param rule the Rule that is to be laid out
  def setRule(rule : Rule) : Unit = {
    this.rule = rule;
  }

  /// Create a GraphLayoutData object containing the layout data for the Rule's RHS graph.
  /// The Rule must have been previously provided to the LiftedGraphLayoutRuleLayoutStrategy (by constructing with the rule or by calling LiftedGraphLayoutRuleLayoutStrategy.setRule).
  /// @return a GraphLayoutData containing the layout data for the output of the Rule.
  def layoutRHS() : GraphLayoutData = {
    strategy.setGraph(this.rule.rhs);
    return strategy.layout()
  }

  /// Create a GraphLayoutData object containing the layout data for the Rule's LHS graph.
  /// The Rule must have been previously provided to the LiftedGraphLayoutRuleLayoutStrategy (by constructing with the rule or by calling LiftedGraphLayoutRuleLayoutStrategy.setRule).
  /// @return a GraphLayoutData containing the layout data for the source graph.
  def layoutLHS() : GraphLayoutData = {
    strategy.setGraph(this.rule.lhs);
    return strategy.layout()
  }
}
