open Core.Std;;

type t = (Symmetry.Xyz.t * Gto.t) list;;

let of_basis b =
  let rec do_work accu = function
    | [] -> accu
    | g::tail ->
        begin
          let new_accu = 
            Symmetry.Xyz.of_symmetry g.Gto.sym 
            |> List.map ~f:(fun x-> (x,g)) 
          in
          do_work (new_accu@accu) tail
        end
  in
  do_work [] b
  |> List.rev
;;


let to_string b =
  List.map ~f:(fun (x,y) -> 
     (Symmetry.Xyz.to_string x)^" "^(Gto.to_string y)
  ) b
  |> String.concat ~sep:"\n"
;;