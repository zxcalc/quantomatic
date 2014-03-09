package quanto.data


case class DStep(parent: Option[DSName],
                 rule: String,
                 matchedVertices: Set[VName],
                 replacedVertices: Set[VName],
                 graph: Graph)

case class Derivation(theory: Theory, head: Graph, steps: Map[DSName,DStep])
