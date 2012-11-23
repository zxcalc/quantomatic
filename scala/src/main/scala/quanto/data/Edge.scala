package quanto.data

case class DuplicateEdgeNameException(name: EName)
extends DuplicateNameException("edge", name)

case class Edge[D](name: EName, data: D = ()) extends NameAndData[EName, D]
