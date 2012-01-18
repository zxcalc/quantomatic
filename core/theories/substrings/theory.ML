structure Str_Theory = GraphicalTheory(
  structure OParam = Str_OGraphParam
  structure VComponentData = Str_OVertexComponentData
  structure EComponentData = EmptyElementComponentData
);

structure Str_Controller = Controller(Str_Theory);

