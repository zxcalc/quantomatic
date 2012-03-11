structure RTechn_Theory = GraphicalTheory(
  structure OParam = RTechn_OGraphParam
  structure VComponentData = RTechn_OVertexComponentData
  structure EComponentData = RTechn_EdgeComponentData
);

structure RTechn_Controller = Controller(RTechn_Theory);
