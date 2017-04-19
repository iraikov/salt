
structure FindRoot =
struct
exception InvalidArrguments
exception NotbracketedBelow
exception NotbracketedAbove

fun brent a b eval eps =
  let open Real
      val maxit = 100
  in
      if a > b then raise InvalidArguments else ();
      let 
          val (fa, fb) = (eval a, eval b)
      in
          if 0.0 < fa *. fb  then if fa < 0.0 then raise NotbracketedBelow else raise NotbracketedAbove else ();
          assert (0. <= eps);
          let val (a, fa, b, fb) = if abs fa < abs fb then (b, fb, a, fa) else (a, fa, b, fb)
              (* The root is between a and b, such that |f(b)| < |f(a)| *)
              fun iter i a b c d fa fb fc mflag =
      if abs_float (b -. a) < eps || fb = 0. || maxit < i then b (* stop condition *)
      else
        let s =
          if fa <> fc && fb <> fc then
            a *. fb *. fc /. ((fa -. fb) *. (fa -. fc)) +. b *. fa *. fc /. ((fb -. fa) *. (fb -. fc)) +. c *. fa *. fb /. ((fc -. fa) *. (fc -. fb)) (* inverse quadratic interpolation *)
          else
            b -. fb *. (b -. a) /. (fb -. fa) (* secant rule *)
        in
        let s, mflag =
          if
            (4. *. s < 3. *. a +. b || b < s) ||
            (mflag && 2. *. abs_float (s -. b) >= abs_float (b -. c)) ||
            (not mflag && 2. *. abs_float (s -. b) >= abs_float (c -. d)) ||
            (mflag && abs_float (b -. c) < eps) ||
            (not mflag && abs_float (c -. d) < eps)
          then 0.5 *. (a +. b), true else s, false
        in
        let fs = eval s in
        (* d <- c; c <- b; *)
        if fa *. fs < 0. then (* in this case, b <- s *)
          if abs_float fa < abs_float fs then iter (i + 1) ~a: s ~b: a ~c: b ~d: c ~fa: fs ~fb: fa ~fc: fb mflag (* switch a, b *)
          else iter (i + 1) ~a ~b: s ~c: b ~d: c ~fa ~fb: fs ~fc: fb mflag
        else (* in this case, a <- s *)
          if abs_float fs < abs_float fb then iter (i + 1) ~a: b ~b: s ~c: b ~d: c ~fa: fb ~fb: fs ~fc: fb mflag (* switch a, b *)
          else iter (i + 1) ~a: s ~b ~c: b ~d: c ~fa: fs ~fb ~fc: fb mflag
    in
    iter 0 ~a ~b ~c: a ~d: nan ~fa ~fb ~fc: fa true

end
