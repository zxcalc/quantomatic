registerTest "Change/list theories" (fn session => (
let
  val _ = writeRequestByParts session ("TS","apple") "invalid_theory"
  val _ = demandErrorResponse session "apple" "BADTHEORY"

  val _ = writeRequestByArgs session ("TL","peach") []
  val theories = readNameListResponse session "peach"
  val _ = if (List.exists (fn x => (x = "red_green")) theories) then () else
        raise test_exp "red_green theory not listed"
  val _ = if (List.exists (fn x => (x = "ghz_w")) theories) then () else
        raise test_exp "ghz_w theory not listed"

  val _ = writeRequestByParts session ("TS","pear") "red_green"
  val _ = demandOkResponse session "pear"
  val _ = writeRequestByParts session ("TG","banana") ""
  val _ = demandNameResponse session "banana" "red_green"

  val _ = writeRequestByParts session ("TS","orange") "red_green_blue"
  val _ = demandOkResponse session "orange"
  val _ = writeRequestByParts session ("TG","kiwi") ""
  val _ = demandNameResponse session "kiwi" "red_green_blue"

  val _ = writeRequestByParts session ("TS","plum") "ghz_w"
  val _ = demandOkResponse session "plum"
  val _ = writeRequestByParts session ("TG","mango") ""
  val _ = demandNameResponse session "mango" "ghz_w"
in () end
));

(* vi:et:sw=2:sts=2
*)

