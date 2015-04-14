open OUnit2

let test_ppx_ifenv _ =
  (* notice none of these tests will even compile if if%const isn't working *)
  assert_equal "BLARG"    @@ if false then "WRONG" else if%const 3=3 then "BLARG" else 3;
  assert_equal "BLAAAARG" @@ if false then "WRONG" else if%const 3=4 then 4 else "BLAAAARG";
  assert_equal "...blarg" @@ if%const 3=3 then (if%const 3=4 then 5 else "...blarg") else 3

let suite = "Test ppx_ifenv" >::: [
    "test_ppx_ifenv" >:: test_ppx_ifenv;
  ]

let _ =
  run_test_tt_main suite
