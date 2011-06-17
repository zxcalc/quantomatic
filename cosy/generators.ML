use "tensor.ML";

val wunit = Tensor.tensor (2,0,1)
[ 0,
  1 ];
  
val wcounit = Tensor.tensor (2,1,0)
[ 1, 0 ];

val wmult = Tensor.tensor (2,2,1)
[ 0, 1, 1, 0,
  0, 0, 0, 1 ];
  
val wcomult = Tensor.tensor (2,1,2)
[ 1, 0,
  0, 1,
  0, 1,
  0, 0 ];
  
val ghzmult = Tensor.tensor (2,2,1)
[ 1, 0, 0, 0,
  0, 0, 0, 1 ];
  
val ghzcomult = Tensor.tensor (2,1,2)
[ 1, 0,
  0, 0,
  0, 0,
  0, 1 ];
  
