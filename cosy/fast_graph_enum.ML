signature GRAPH_ENUM =
sig
  structure EqClassTab : EQ_CLASS_TAB
end

functor FastGraphEnum(EqClassTab : EQ_CLASS_TAB) : GRAPH_ENUM =
struct
  structure EqClassTab = EqClassTab
  structure Theory = EqClassTab.Theory
  structure G = Theory.Graph
  
  (****)
end