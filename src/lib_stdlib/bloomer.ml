(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type 'a t = {
  hash : 'a -> bytes;
  hashes : int;
  index_bits : int;
  countdown_bits : int;
  filter : bytes;
}

(* Reads [bits] bits of [bytes] at offset [ofs] as an OCaml int in big endian order.
   The function proceeds by iteratively blitting the bytes overlapping the sought
   bit interval into [v]. The superfluous bits at the beginning and at the end
   are them removed from [v], yielding the returned value.
 *)
let peek bytes ofs bits =
  if bits <= 0 then invalid_arg "Bloomer.peek: non positive bits value" ;
  if ofs < 0 then invalid_arg "Bloomer.peek: negative offset" ;
  if bits > Sys.word_size - 2 then
    invalid_arg "Bloomer.peek: indexes out of bounds" ;
  if bits + ofs > Bytes.length bytes * 8 then
    invalid_arg "Bloomer.peek: indexes out of bounds" ;
  let first = ofs / 8 in
  let last = first + (((ofs mod 8) + bits + 7) / 8) in
  let v = ref 0 in
  for i = last - 1 downto first do
    v := (!v lsl 8) lor Char.code (Bytes.get bytes i)
  done ;
  v := !v lsr (ofs mod 8) ;
  v := !v land ((1 lsl bits) - 1) ;
  !v

(* blits [bits] bits of [bytes] at offset [ofs] from an OCaml int in big endian order *)
let poke bytes ofs bits v =
  if v lsr bits <> 0 then invalid_arg "Bloomer.poke: value too large" ;
  if bits <= 0 then invalid_arg "Bloomer.poke: non positive bits value" ;
  if ofs < 0 then invalid_arg "Bloomer.poke: negative bits value" ;
  if bits > Sys.word_size - 2 then
    invalid_arg "Bloomer.peek: indexes out of bounds" ;
  if bits + ofs > Bytes.length bytes * 8 then
    invalid_arg "Bloomer.poke: indexes out of bounds" ;
  let first = ofs / 8 in
  let last = first + (((ofs mod 8) + bits + 7) / 8) in
  let cur = ref 0 in
  for i = last - 1 downto first do
    cur := (!cur lsl 8) lor Char.code (Bytes.get bytes i)
  done ;
  let mask = lnot (((1 lsl bits) - 1) lsl (ofs mod 8)) in
  let v = !cur land mask lor (v lsl (ofs mod 8)) in
  for i = first to last - 1 do
    Bytes.set bytes i (Char.chr ((v lsr ((i - first) * 8)) land 0xFF))
  done

let%test_unit "random_read_writes" =
  let bytes = Bytes.make 45 '\000' in
  let poke_et_peek ofs len v =
    poke bytes ofs len v ;
    assert (peek bytes ofs len = v)
  in
  for _ = 0 to 100_000 do
    let ofs = Random.int 320 in
    let len = Random.int 29 + 1 in
    let v = Random.int (1 lsl len) in
    poke_et_peek ofs len v
  done ;
  poke_et_peek 350 10 0x3FF ;
  poke_et_peek 355 5 0x1F ;
  poke_et_peek 350 10 0 ;
  poke_et_peek 355 5 0 ;
  try
    poke_et_peek 355 6 0 ;
    assert false
  with _ -> ()

let%test_unit "sequential_read_writes" =
  let bytes = Bytes.make 45 '\000' in
  let bits = Bytes.length bytes * 8 in
  for _ = 0 to 10_000 do
    let rec init ofs acc =
      if ofs >= bits then List.rev acc
      else
        let len = min (Random.int 29 + 1) (bits - ofs) in
        let v = Random.int (1 lsl len) in
        poke bytes ofs len v ;
        assert (peek bytes ofs len = v) ;
        init (ofs + len) ((ofs, len, v) :: acc)
    in
    List.iter
      (fun (ofs, len, v) -> assert (peek bytes ofs len = v))
      (init 0 [])
  done

let%test_unit "read_over_write" =
  (* Check that non-overlapping writes really do not overlap. *)
  let bytes = Bytes.make 45 '\000' in
  let bits = Bytes.length bytes * 8 in
  let random_disjoint_writes () =
    let width = 1 lsl Random.int 3 in
    let indices = bits / width in
    let i1 = Random.int indices in
    let i2 = (i1 + 1 + Random.int (indices - 1)) mod indices in
    let i1 = i1 * width in
    let i2 = i2 * width in
    assert (i1 <> i2) ;
    let v1 = Random.int (1 lsl width) in
    let v2 = Random.int (1 lsl width) in
    poke bytes i1 width v1 ;
    poke bytes i2 width v2 ;
    assert (peek bytes i1 width = v1)
  in
  for _ = 0 to 10_000 do
    random_disjoint_writes ()
  done

let create ~hash ~hashes ~index_bits ~countdown_bits =
  if index_bits <= 0 || index_bits > 24 then
    invalid_arg "Bloomer.create: invalid value for index_bits" ;
  if countdown_bits <= 0 || countdown_bits > 24 then
    invalid_arg "Bloomer.create: invalid value for countdown_bits" ;
  let filter =
    Bytes.make ((((1 lsl index_bits) * countdown_bits) + 7) / 8) '\000'
  in
  {hash; hashes; index_bits; countdown_bits; filter}

let mem {hash; hashes; index_bits; countdown_bits; filter} x =
  let h = hash x in
  try
    for i = 0 to hashes - 1 do
      let j = peek h (index_bits * i) index_bits in
      if peek filter (j * countdown_bits) countdown_bits = 0 then raise Exit
    done ;
    true
  with Exit -> false

let add {hash; hashes; index_bits; countdown_bits; filter} x =
  let h = hash x in
  for i = 0 to hashes - 1 do
    let j = peek h (index_bits * i) index_bits in
    poke filter (j * countdown_bits) countdown_bits ((1 lsl countdown_bits) - 1)
  done

let rem {hash; hashes; index_bits; countdown_bits; filter} x =
  let h = hash x in
  for i = 0 to hashes - 1 do
    let j = peek h (index_bits * i) index_bits in
    poke filter (j * countdown_bits) countdown_bits 0
  done

let countdown {hash = _; hashes = _; index_bits; countdown_bits; filter} =
  for j = 0 to (1 lsl index_bits) - 1 do
    let cur = peek filter (j * countdown_bits) countdown_bits in
    if cur > 0 then poke filter (j * countdown_bits) countdown_bits (cur - 1)
  done

let clear {hash = _; hashes = _; index_bits = _; countdown_bits = _; filter} =
  Bytes.fill filter 0 (Bytes.length filter) '\000'

let%test_unit "consistent_add_mem_countdown" =
  for _ = 0 to 100 do
    let index_bits = Random.int 16 + 1 in
    let hashes = Random.int 7 + 1 in
    let countdown_bits = Random.int 5 + 1 in
    let hash v =
      Bytes.init
        (((hashes * index_bits) + 7) / 8)
        (fun i -> Char.chr (Hashtbl.hash (v, i) mod 256))
    in
    let bloomer = create ~hash ~index_bits ~hashes ~countdown_bits in
    let rec init n acc =
      if n = 0 then acc
      else
        let x = Random.int (1 lsl 29) in
        add bloomer x ;
        assert (mem bloomer x) ;
        init (n - 1) (x :: acc)
    in
    let all = init 1000 [] in
    for _ = 0 to (1 lsl countdown_bits) - 2 do
      List.iter (fun x -> assert (mem bloomer x)) all ;
      countdown bloomer
    done ;
    List.iter (fun x -> assert (not (mem bloomer x))) all
  done

let%test_unit "false_positive_rate" =
  (* We acknowledge the results published in "On the false-positive
     rate of Bloom filters" (Information Processing Letters, volume
     108, issue 4, 2008, pages 210-213) stating that the model formula
     below is wrong.

     However, we still use the original approximation made by Bloom to
     check the behaviour of this implementation, as it is close enough
     for security yet much simpler to compute. *)
  let runs =
    [| (18, 4);
       (18, 6);
       (18, 8);
       (18, 10);
       (20, 2);
       (20, 4);
       (20, 6);
       (20, 8);
       (20, 9);
       (20, 10);
       (20, 11);
       (20, 12);
       (20, 13);
       (20, 14);
       (21, 2);
       (21, 3);
       (21, 4);
       (21, 5);
       (22, 2);
       (22, 3);
       (22, 4);
       (22, 5);
       (22, 6);
       (22, 8) |]
  in
  let steps = 995 in
  let init_samples = 5_000 in
  let samples_per_step = 1_000 in
  let data =
    Array.map
      (fun (index_bits, hashes) ->
        let countdown_bits = 1 in
        let hash v =
          Bytes.init
            (((hashes * index_bits) + 7) / 8)
            (fun i -> Char.chr (Hashtbl.hash (v, i) mod 256))
        in
        let bloomer = create ~hash ~index_bits ~hashes ~countdown_bits in
        let (add, cur) =
          let cur = ref 0 in
          ( (fun n ->
              for _ = 1 to n do
                add bloomer !cur ; incr cur
              done),
            fun () -> !cur )
        in
        add init_samples ;
        ( float (Bytes.length bloomer.filter) /. 1024.,
          index_bits,
          hashes,
          Array.init steps
          @@ fun i ->
          add samples_per_step ;
          let n = init_samples + ((i + 1) * samples_per_step) in
          let expected_fp_proba =
            let e = 2.718281828459045 in
            ( 1.
            -. (e ** (-.float hashes *. float n /. float (1 lsl index_bits)))
            )
            ** float hashes
          in
          let actual_proba =
            let falses = ref 0 in
            for j = 1 to 500 do
              if mem bloomer (cur () + j) then incr falses
            done ;
            float !falses /. 500.
          in
          ( if abs_float (expected_fp_proba -. actual_proba) >= 0.1 then
            let message =
              Format.asprintf
                "wrong false positive rate for n=%d, m=%d,k=%d, expected %g, \
                 got %g"
                n
                (1 lsl index_bits)
                hashes
                expected_fp_proba
                actual_proba
            in
            failwith message ) ;
          (expected_fp_proba, actual_proba) ))
      runs
  in
  match Sys.getenv_opt "BLOOMER_TEST_GNUPLOT_PATH" with
  | Some path ->
      for run = 0 to Array.length runs - 1 do
        let (kb, index_bits, hashes, values) = data.(run) in
        (let fp = open_out (Format.asprintf "%s/run_%02d.plot" path run) in
         Printf.fprintf
           fp
           "set title 'false positive rate (bits=%d (%g KiB), hashes=%d)'\n%!"
           (1 lsl index_bits)
           kb
           hashes ;
         Printf.fprintf fp "set xlabel 'insertions'\n" ;
         Printf.fprintf fp "set ylabel 'rate'\n" ;
         Printf.fprintf fp "set yrange [0:1]\n" ;
         Printf.fprintf fp "set terminal 'png' size 800,600\n" ;
         Printf.fprintf fp "set output 'run_%02d.png'\n" run ;
         Printf.fprintf
           fp
           "plot 'run_%02d.dat' using 1:2 title 'expected', 'run_%02d.dat' \
            using 1:3 title 'obtained'\n\
            %!"
           run
           run ;
         close_out fp) ;
        let fp = open_out (Format.asprintf "%s/run_%02d.dat" path run) in
        for step = 0 to steps - 1 do
          Printf.fprintf
            fp
            "%d %f %f\n"
            (init_samples + (step * samples_per_step))
            (fst values.(step))
            (snd values.(step))
        done ;
        flush fp ;
        close_out fp
      done
  | None ->
      Format.eprintf
        "Set the BLOOMER_TEST_GNUPLOT_PATH to a directory to get some human \
         readable test results."
