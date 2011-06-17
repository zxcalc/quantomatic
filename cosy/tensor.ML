signature TENSOR =
sig
  type T
  type perm
  structure Tab : TABLE
  val pow : int -> int -> int
  val gcd : int -> int -> int
  val get : T -> (int * int) -> int
  val dim : T -> int
  val ord : T * T -> order
  val lower_index_count : T -> int
  val upper_index_count : T -> int
  val tensor : (int * int * int) -> int list -> T
  val tensorf : (int * int * int) -> ((int * int) -> int) -> T
  val id : int -> int -> T
  val to_string : T -> string
  val print : T -> unit
  val product : T -> T -> T
  val reduce : T -> int * T
  val all_perms : T -> (perm * perm) Tab.table
  val normalise : T -> (int * perm * perm) * T
  val contract : (int * int) -> T -> T
  val contract2 : (int * int) -> T -> T -> T
  val compose : T -> T -> T
  val decompose_index : int -> int -> int -> int list
  val recompose_index : int -> int list -> int
  val scalar_compare : T -> T -> (int * int) option
  val scalar_eq : T -> T -> bool
  val perm_scalar_eq : T -> T -> bool
end

structure TensorKey : KEY =
struct
  type key = ((int * int * int) * int vector)
  fun ord (  ((d,l,u),v),  ((d1,l1,u1),v1)  ) =
    case (prod_ord int_ord (prod_ord int_ord int_ord)) (  (d,(l,u))  ,  (d1,(l1,u1))  )
      of EQUAL => Vector.collate int_ord (v,v1)
       | comp => comp
end

structure Tensor : TENSOR = 
struct
  type T = TensorKey.key
  type perm = int list
  val ord = TensorKey.ord
  structure Tab = Table(TensorKey)
  
  exception NegativeExponent
  fun is_zero i = (i = 0)
  
  (* fast exponentiation *)
  fun pow x 0 = 1
    | pow x 1 = x
    | pow x n = if n < 0 then raise NegativeExponent else
          (if (n mod 2 = 0) then pow (x*x) (n div 2)
                            else x * (pow (x*x) (n div 2)))
  

  fun gcd a 0 = a
    | gcd 0 a = a
    | gcd a b = gcd b (a mod b)
    
  
    
    
  fun dim ((d,_,_),_) = d;
  fun lower_index_count ((_,l,_),_) = l
  fun upper_index_count ((_,_,u),_) = u
    
  fun get ((d,l,u),vect) (row,col) = Vector.sub (vect, (pow d l) * row + col);
  fun tensor (d,l,u) lst = ((d,l,u), Vector.fromList lst);
  fun tensorf (d,l,u) f = let
    val offset = pow d l
  in ((d,l,u), Vector.tabulate (pow d (u+l), (fn i => f(i div offset, i mod offset))))
  end
  
  fun id d n = tensorf (d,n,n) (fn (row,col) => if row=col then 1 else 0)
  
  (* reduce to scalar-canonical form *)
  fun reduce (size, vect) = case Vector.foldr (uncurry gcd) 0 vect
                                 of 0 => (1, (size,vect))
                                  | n => (n, (size, Vector.map (fn m => m div n) vect))
  
  fun get_from_product t1 (t2 as ((d2, l2, u2), v2)) (row,col) = let
    val uoff = pow d2 u2
    val loff = pow d2 l2
  in (get t1 (row div uoff, col div loff)) *
     (get t2 (row mod uoff, col mod loff))
  end
  
  fun product (t1 as ((d1, l1, u1), v1)) (t2 as ((d2, l2, u2), v2)) =
    tensorf (d1, l1+l2, u1+u2) (get_from_product t1 t2)
  
  fun decompose_index d len index = let
    fun dec 0 idx = []
      | dec k idx = (idx mod d)::(dec (k-1) (idx div d))
  in rev (dec len index)
  end
  
  fun recompose_index d lst = List.foldl (fn (part, rest) => part + d * rest) 0 lst
  
  (* insert element "k" in the list before "bef" *)
  fun list_ins 0 k [] = [k]
    | list_ins _ _ [] = []
    | list_ins bef k (l::ls) = if bef = 0 then k::l::(list_ins (bef-1) k ls)
                               else l::(list_ins (bef-1) k ls)
  
  
  (* trace the j-th input (lower index) to the k-th output (upper index) *)
  fun contractf f (j,k) (d,l,u) = let
    fun tr (row, col) = let
      val rowdec = decompose_index d (u-1) row
      val coldec = decompose_index d (l-1) col
      fun sum 0 = 0
        | sum i = f (recompose_index d (list_ins k (i-1) rowdec),
                     recompose_index d (list_ins j (i-1) coldec))
                  + sum (i-1)
    in sum d
    end
  in
    tensorf (d, l-1, u-1) tr
  end
  
  fun contract io (t as (size,_)) = contractf (get t) io size 
  
  (* connect the k-th output of t1 to the j-th input of t2 *)
  fun contract2 (j,k) (t2 as ((_,l2,u2),_)) (t1 as ((d,l1,u1),_)) =
    contractf (get_from_product t1 t2) (l1+j,k) (d,l1+l2,u1+u2)
    
  fun compose (t2 as ((d,l2,u2),_)) (t1 as ((_,l1,_),_)) = let
    fun comp_get 0 _ = 0
      | comp_get n (row,col) = ((get t2 (row,n-1)) * (get t1 (n-1,col))) +
                               (comp_get (n-1) (row,col))
  in tensorf (d,l1,u2) (comp_get (pow d l2))
  end
  
  fun scalar_compare (_,v1) (_,v2) = let
    val nz1 = Vector.findi (fn (_,n) => not (is_zero n)) v1
    val nz2 = Vector.findi (fn (_,n) => not (is_zero n)) v2
  in case (nz1,nz2)
     of (NONE,NONE) => SOME (1,1)
      | (NONE,_) => NONE
      | (_,NONE) => NONE
      | (SOME (p1,n1), SOME (p2,n2)) =>
        if (p1 = p2) andalso 
        (Vector.foldri
            (fn (i,e,b) => (e*n2 = (Vector.sub (v2,i))*n1) andalso b)
            true
            v1) then SOME (n2,n1)
        else NONE
  end
  
  fun apply_perm perm lst = map (fn i => List.nth (lst,i)) perm
  
  fun permrow_get (t as ((d,_,_),_)) perm (row,col) = let
    val prow = recompose_index d (apply_perm perm (decompose_index d (length perm) row))
  in get t (prow, col)
  end
  
  fun permcol_get (t as ((d,_,_),_)) perm (row,col) = let
    val pcol = recompose_index d (apply_perm perm (decompose_index d (length perm) col))
  in get t (row,pcol)
  end
  
  fun permrowcol_get (t as ((d,_,_),_)) permrow permcol (row,col) = let
    val prow = recompose_index d (apply_perm permrow (decompose_index d (length permrow) row))
    val pcol = recompose_index d (apply_perm permcol (decompose_index d (length permcol) col))
  in get t (prow, pcol)
  end
  
  fun perm_matrix d perm = let
    val n = length perm
    fun ptens (row,col) =
      if (recompose_index d
           (apply_perm perm
             (decompose_index d n row)
           )) = col then 1 else 0
  in tensorf (d,n,n) ptens
  end
  
  fun perm_matrix d perm = let val n = length perm
  in tensorf (d,n,n) (permcol_get (id d n) perm)
  end
  
  fun perms size = let
    fun lst 0 = [] | lst n = (n-1)::(lst (n-1))
    fun prm [] [] = [[]]
      | prm _ [] = []
      | prm h (x::xs) = (map (fn p=>x::p) (prm [] ((rev h)@xs)))
                          @ (prm (x::h) xs)
  in prm [] (lst size)
  end
  
  fun scalar_eq t1 t2 = case scalar_compare t1 t2
                          of SOME _ => true | NONE => false
  
  
  fun compare_all comp [] _ = []
    | compare_all comp _ [] = []
    | compare_all comp (x::xs) (y::ys) = if comp x y then
                                         (x,y)::(compare_all comp (x::xs) ys @
                                                 compare_all comp xs ys)
                                         else (compare_all comp (x::xs) ys @
                                               compare_all comp xs ys)
  
  (*fun perm_scalar_eq (t1 as ((d,l,u),_)) t2 = let
    val lst1 = map (fn p => tensorf (d,l,u) (permrow_get t1 p)) (perms u)
    val lst2 = map (fn p => tensorf (d,l,u) (permcol_get t2 p)) (perms l)
  in (compare_all scalar_eq lst1 lst2) <> []
  end*)
  
  fun all_perms (t as ((d,l,u),_)) = let
    fun f prow pcol tab = tab |> Tab.update (tensorf (d,l,u) (permrowcol_get t prow pcol), (prow,pcol))
  in fold_product f (perms u) (perms l) (Tab.empty)
  end
  
  fun normalise t = let
    val (s, t') = reduce t
    val perm_table = all_perms t'
    val SOME t' = Tab.min_key perm_table
    val SOME (prow,pcol) = Tab.lookup perm_table t'
  in ((s, prow, pcol), t')
  end
  
  fun perm_scalar_eq t1 t2 = let
    val (_,t1') = normalise t1
    val (_,t2') = normalise t2
  in ord (t1',t2') = EQUAL
  end
  
  fun pad width s = let
    fun space 0 = "" | space w = " " ^ space (w-1)
    in s ^ space (width - (size s))
  end
  
  fun to_string ((d,l,u),v) = let
    val width = (size (Int.toString (Vector.foldr Int.max 0 v))) + 1
    val rowsize = pow d l
    fun str (i, num, rest) =
      (if (i <> 0) andalso ((i mod rowsize) = 0) then "]\n[ " else "") ^
      (pad width (Int.toString num)) ^ rest
  in
    "[ " ^ (Vector.foldri str "" v) ^ "]\n\n"
  end
  
  fun print t = TextIO.print (to_string t)
end


val t1 = Tensor.tensor (2,2,1)
[ 0, 4, 0,  8,
 16,64,32,128]
val t2 = Tensor.tensor (2,2,1) [0,0,8,16,32,64,128,256]
val t3 = Tensor.tensor (2,2,1) [2,4,8,16,32,64,128,255]
val X = Tensor.tensor (2,1,1) [0,1,1,0]
val vect = Tensor.tensor (2,0,1) [1,2]
val vect2 = Tensor.contract2 (0,0) X vect

val _ = Tensor.print t1






