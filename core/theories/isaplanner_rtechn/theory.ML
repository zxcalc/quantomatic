structure RTechn_Theory = GraphicalTheory(
  structure OParam = RTechn_OGraphParam
  structure VComponentData = RTechn_OVertexComponentData
  structure EComponentData = EmptyElementComponentData
);

structure RTechn_Controller = Controller(RTechn_Theory);
