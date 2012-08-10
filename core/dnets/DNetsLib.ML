signature TDNET_LIBRARY =
sig
  
  val maps2 : ('a -> 'b -> 'b list) -> 'a list -> 'b list -> 'b list
  val maps3 : ('a -> 'b -> 'c list) -> 'a -> 'b list -> 'c list

  val is_contained : ('a * 'a -> bool) -> 'a -> 'a list -> bool
  val rm_duplicates : ('a * 'a -> bool) -> 'a list -> 'a list
  val rm_element : ('a * 'a -> bool) -> 'a -> 'a list -> 'a list
  val sub_x_y : ('a * 'a -> bool) -> 'a list -> 'a list -> 'a list

end


structure TDNet_Library : TDNET_LIBRARY =
struct

  fun maps2 _ [] [] = []
    | maps2 f (x::xs) (y::ys) = f x y @ maps2 f xs ys
    | maps2 _ _ _ = raise ListPair.UnequalLengths;

  fun maps3 _ _ [] = []
    | maps3 f x (y::ys) = f x y @ maps3 f x ys

  fun is_contained _ x [] = false
    | is_contained f x (y::[]) = if f(x,y) then true else false
    | is_contained f x (y::ys) = if f(x,y) then true else is_contained f x ys

  fun rm_duplicates _ [] = []
    | rm_duplicates _ (x::[]) = x::[]
    | rm_duplicates f (x::xs) = if is_contained f x xs then rm_duplicates f xs else x :: (rm_duplicates f xs)

  fun rm_element _ x [] = []
    | rm_element f x (y::[]) = if f(x,y) then [] else y::[]
    | rm_element f x (y::ys) = if f(x,y) then ys else y::(rm_element f x ys)

  fun sub_x_y _ x [] = x
    | sub_x_y f x (y::[]) = if is_contained f y x then rm_element f y x else x
    | sub_x_y f x (y::ys) = if is_contained f y x then sub_x_y f (rm_element f y x) ys else sub_x_y f x ys


end