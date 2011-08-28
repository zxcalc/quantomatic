structure RGB_Theory = GraphicalTheory(
structure OParam = RGB_OGraphParam
structure VComponentData = RGB_OVertexComponentData
structure EComponentData = EmptyElementComponentData
) : GRAPHICAL_THEORY;

structure RGB_Controller = Controller(ControllerState(RGB_Theory));
