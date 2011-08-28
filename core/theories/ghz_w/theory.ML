structure GHZW_Theory = GraphicalTheory(
structure OParam = GHZW_OGraphParam
structure VComponentData = GHZW_OVertexComponentData
structure EComponentData = EmptyElementComponentData
) : GRAPHICAL_THEORY;

structure GHZW_Controller = Controller(ControllerState(GHZW_Theory));
