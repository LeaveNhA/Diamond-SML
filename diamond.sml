fun rangePrime 0 = [ 0 ]
  | rangePrime x = x :: rangePrime (x - 1)

fun reverse nil = nil
  | reverse (h::t) = reverse t @ [ h ]

val range =
    (reverse o rangePrime)

fun head xs =
    case xs of
        head::tail => head

fun identity i =
    i

val constant =
 fn v => fn _ => v

val applyNth =
 fn fPrime =>
    fn f =>
       fn n =>
          fn (v, (xs, i)) =>
             if i = n then
                 (xs @ [f(v)], i + 1)
             else
                 (xs @ [fPrime(v)], i + 1)

fun changeNth x =
    fn xs =>
       fn n =>
          foldl (applyNth identity (constant x) n) ([], 1) xs;

val withIndexes =
 fn xs =>
    case (foldl (fn (v, (xs, i)) => (xs @ [(v, i)], i + 1)) ([], 1) xs) of
        (xsWithIndexes, n) => xsWithIndexes

val dec =
 fn i => i - 1

val emptyDataS =
 fn xs =>
    map (map (constant #" ") o range o dec) (map (constant (List.length xs)) xs)

val multiplyWith =
 fn f =>
    fn x => (f(x), x)

val replaceWithValue =
 fn (xs : char list, (v : int , i : int)) =>
    case changeNth (Char.chr v) xs i of
        (v, _) => v

val mirror =
 fn (v, rv) => (reverse(rv), v)

val mirrorR =
 fn (v, rv) => (v, reverse(rv))

val concatMirrorData =
 fn (a, b) =>
    case b of
        h::tail =>  a @ tail
     | _ => a @ b

fun rows (input: string) =
    (map implode o concatMirrorData o mirrorR o multiplyWith identity o map concatMirrorData o map mirror o ListPair.map identity o multiplyWith identity o map replaceWithValue o ListPair.map identity o multiplyWith(emptyDataS) o withIndexes o map (fn i => i + 65) o range o (fn i => i - 65) o head o map Char.ord o String.explode)(input)
