open OUnit2

let test_ppx_ifenv _ =
  (* set in myocamlbuild.ml *)
  assert_equal "BLARG" @@ if false then "WRONG" else if%const a then "BLARG" else this_is_an_error

let suite = "Test ppx_ifenv" >::: [
    "test_ppx_ifenv" >:: test_ppx_ifenv;
  ]

let _ =
  run_test_tt_main suite
