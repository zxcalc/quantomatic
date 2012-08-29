structure Petri_Theory = GraphicalTheory(
structure OParam = Petri_OGraphParam
structure VComponentData = Petri_OVertexComponentData
structure EComponentData = EmptyElementComponentData
) : GRAPHICAL_THEORY;

structure Petri_Controller = Controller(Petri_Theory);
