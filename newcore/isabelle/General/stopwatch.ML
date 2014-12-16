(*  Title:      Pure/General/stopwatch.ML
    Author:     Michael James Bradley

The stopwatch structure is designed just to make timing functions simpler and hopefully
more intuitive.
*)
signature STOPWATCH =
sig
  type T; (*The type of the stop watch*)
  (*The direction of the stop watch
  If it is up, its time is increasing. Otherwise it is decreasing.*)
  datatype Direction = UP | DOWN
  (*Create a brand new stop watch. Set to time up and has 0 time on it.*)
  val new : T
  (*Start the timer on the stop watch. If it is already running, this does
  nothing.*)
  val start : T -> T
  (*Pause the timer. This returns the current time on the stop watch,
  and does not reset the time.*)
  val pause : T -> (Time.time * T)
  (*Stop the timer. This returns the amount of time on the stop watch,
  and does reset it to 0*)
  val stop : T -> (Time.time * T)
  (*Reset the timer to 0. If it is still timing, it WILL continue timing.*)
  val reset : T -> T
  (*Read the current time on the stop watch. If the stop watch is active,
  then time will continue to pass effectively. If it is paused, it will stay paused.*)
  val check : T -> Time.time
  (*Set the time on the stop watch to the given time. If the stop watch
  is active, it will continue to add time effectively to this. If it is
  paused, it will stay paused with the time set to the given value*)
  val set : T ->
    Time.time -> (*The time to set the stop watch to*)
    T
  (*Set the stop watch to time up or down. That is, if the time is increasing
  or decreasing when started. If it is already timing, then it will start
  immediately in the new direction.*)
  val set_direction : T ->
    Direction -> (*Whether it should time up or down*)
    T
  (*Check the direction of the stop watch*)
  val check_direction : T -> Direction
end;

(*The structure to implement the stopwatch:*)
structure Stopwatch :> STOPWATCH =
struct
  datatype Direction = UP | DOWN (*The direction of the stop watch*)
  (*(is timing,the time on the stop watch,the time since the last reading, the direction)*)
  type T = (bool * Time.time * Time.time * Direction) (*The type of the stop watch*)
  (*Create a brand new stop watch! Set to time up and has 0 time on it.*)
  val new = (false,Time.zeroTime,Time.zeroTime,UP)
  (*Start the timer on the stop watch*)
  fun start (false,dt,_,d) = (true,dt,Time.now(),d)
    | start sw = sw
  (*Pause the timer. This returns the current time on the stop watch,
  and does not reset the time.*)
  fun pause (false,dt,lt,d) = (dt,(false,dt,lt,d))
    | pause (true,dt,lt,d) =
      let
        val diff = Time.- (Time.now(),lt);
        val newt = case d of
            UP => Time.+ (dt,diff) (*Add to the previously displayed time*)
          | DOWN => Time.-(dt,diff) (*Subtract from the previously displayed time*)
      in
        (newt,(false,newt,Time.zeroTime,d))
      end
  (*Reset the timer to 0. If it is still timing, it will continue timing.*)
  fun reset (timing,_,_,d) = (timing,Time.zeroTime,Time.now(),d)
  (*Stop the timer. This returns the amount of time on the stop watch,
  and does reset it to 0*)
  fun stop sw =
    let
      val (t,sw') = pause sw;
      val sw'' = reset sw'
    in
      (t,sw'')
    end
  (*Read the current time on the stop watch. If the stop watch is active,
  then time will continue to pass effectively. If it is paused, it will stay paused.*)
  fun check (false,dt,_,_) = dt
    | check (true,dt,lt,d) =
      let
        val diff = Time.- (Time.now(),lt);
        val newt = case d of
            UP => Time.+ (dt,diff) (*Add to the previously displayed time*)
          | DOWN => Time.-(dt,diff) (*Subtract from the previously displayed time*)
      in
        newt
      end
  (*Set the time on the stop watch to the given time. If the stop watch
  is active, it will continue to add time effectively to this. If it is
  paused, it will stay paused with the time set to the given value*)
  fun set (timing,_,_,d) time = (timing,time,Time.now(),d)
  (*Set the stop watch to time up or down. That is, if the time is increasing
  or decreasing when started. If it is already timing, then it will start
  immediately in the new direction.*)
  fun set_direction (false,dt,lt,_) dir = (false,dt,lt,dir)
    | set_direction (true,dt,lt,d) dir =
      let
        val now = Time.now();
        val diff = Time.- (now,lt);
        val newt = case d of
            UP => Time.+ (dt,diff) (*Add to the previously displayed time*)
          | DOWN => Time.-(dt,diff) (*Subtract from the previously displayed time*)
      in
        (true,newt,now,dir)
      end
  (*Check the direction of the stop watch*)
  fun check_direction (_,_,_,d) = d
end;
