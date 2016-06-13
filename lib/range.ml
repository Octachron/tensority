type ('n_in, 'n_out) t = { start:int; stop:int; step:int }

let create ~start ~stop ~step ~len =
  let diff = Nat.to_int stop - Nat.to_int start in
  let dyn_len = diff / step and len = Nat.to_int len in
  if len <> dyn_len then
    raise @@ Signatures.Dimension_error
      ("Slices.range.create", dyn_len , len )
  else
    {start= Nat.to_int start ; stop= Nat.to_int stop; step }

let start r = Nat.Unsafe.create r.start
let stop r = Nat.Unsafe.create r.stop
let step r = r.step
let len r = Nat.Unsafe.create @@ (r.stop - r.start) / r.step
let compose r1 r2 =
  { start = r1.start + r2.start
  ; stop = r1.start + r2.stop
  ; step = r1.step * r2.step
  }
let transpose r p = Nat.Unsafe.create @@ r.start + r.step * Nat.to_int p

let (--) start stop len = create ~start ~stop ~len ~step:1
let (-->) start stop (step,len) = create ~start ~stop ~len ~step

let pp ppf r =
  Format.fprintf ppf "@[[%a->%a by %d (%a)]@]"
    Nat.pp (start r) Nat.pp (stop r)
    (step r)
    Nat.pp (len r)

let show r=
  Format.asprintf "%a" pp r
