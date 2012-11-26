package quanto.data

case class Edge[D](name: EName, data: D = ()) extends NameAndData[EName, D]
