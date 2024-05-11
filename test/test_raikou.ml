open OUnit2
open Raikou
open Raikou.Vec3

let v1 = Vec3.create 1. 2. 3.
let v2 = Vec3.create 2. 4. 5.

let tests = "test suite" >::: [
  "Vec3 create" >:: (fun _ -> 
      assert_equal v1.x 1.; 
      assert_equal v1.y 2.; 
      assert_equal v1.z 3.
    );
  
  "Vec3 + " >:: (fun _ -> 
        assert_equal (v1 +| v2) (Vec3.create 3. 6. 8.)
      ); 
  
  "Vec3 - " >:: (fun _ -> 
        assert_equal (v1 -| v2) (Vec3.create (-1.) (-2.) (-2.))
      ); 
  
  "Vec3 * " >:: (fun _ -> 
        assert_equal (v1 *| 2.) (Vec3.create 2. 4. 6.)
      ); 
  
  "Vec3 / " >:: (fun _ -> 
        assert_equal (v1 /| 2.) (Vec3.create 0.5 1. 1.5)
      ); 
  
  "Vec3 dot " >:: (fun _ -> 
        assert_equal (Vec3.dot v1 v1) 14.
      );
  
  "Vec3 r2 " >:: (fun _ -> 
        assert_equal (Vec3.length_square v1) 14.
      );
  
  "Vec3 r " >:: (fun _ -> 
        assert_equal (Vec3.length v1) (Float.sqrt 14.)
      );
  
  "Vec3 cross " >:: (fun _ -> 
        assert_equal (Vec3.cross v1 v2) (Vec3.create (-2.) 1. 0.)
      );

  "Vec3 neg" >:: (fun _ -> 
      let v3 = Vec3.neg v1 in
        (assert_equal v3.x (-1.); 
         assert_equal v3.y (-2.); 
         assert_equal v3.z (-3.)
        )
      );
  
  "Vec3 unit " >:: (fun _ -> 
        assert_equal (Vec3.length (Vec3.unit v1)) (1.)
      );

]
let _ = run_test_tt_main tests
