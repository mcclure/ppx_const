open OUnit2

let test_ppx_ifenv _ =
  (* set in myocamlbuild.ml *)
  assert_equal "42" [%ifenv "PPX_GETENV_CHECK"]

let suite = "Test ppx_ifenv" >::: [
    "test_ppx_ifenv" >:: test_ppx_ifenv;
  ]

let _ =
  run_test_tt_main suite
