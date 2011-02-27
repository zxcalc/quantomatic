(*-------------------------------------------------------*)

structure StrGraphDotWriter : OUTPUT_WRITER 
			      where type graph = StrGraph.T
 = BasicWriter (OutputDot (StrGraph));

structure StrGraphDot : OUTPUT
			      where type graph = StrGraph.T
 = OutputDot (StrGraph);


(*-------------------------------------------------------*)

structure StrGraphXMLWriter : OUTPUT_WRITER 
			      where type graph = StrGraph.T
 = BasicWriter (OutputXML (StrGraph));

structure StrGraphXML : OUTPUT
			      where type graph = StrGraph.T
 = OutputXML (StrGraph);

