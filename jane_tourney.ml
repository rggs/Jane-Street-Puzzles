open Format
open Array
open List

let tourney = [1.;16.;8.;9.;5.;12.;4.;13.;6.;11.;3.;14.;7.;10.;2.;15.];;

let odds x =
  function y -> if x > y then 1.0-.(x/.x+.y) else y/.(x+.y);;

printf "The odds are %f" (odds 2. 15.);;

let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l;;

let rec index l a =
  match l with
  | [] -> -1
  | x::xs -> if x != a then 1 + index xs a else 0;;

let rec fold_until f acc n = function
  | [] -> (acc, [])
  | h :: t as l -> if n = 0 then (acc, l)
              else fold_until f (f acc h) (n - 1) t

let slice list i k =
  let _, list = fold_until (fun _ _ -> []) [] i list in
  let taken, _ = fold_until (fun acc h -> h :: acc) [] (k - i + 1) list in
  List.rev taken;;


let seed_odds l seed =
  let team_ind = index tourney seed
  and rounds = [2;4;8;16]
  
  and round_odds = make_matrix 4 16 0.0 in
  
  for r = 0 to (length rounds)-1 do
    let groups = (length tourney) / (nth rounds r) in

    for i = 0 to groups-1 do
      let teams = slice tourney (i*(nth rounds r)) ((i+1)*(nth rounds r)) in
      for t = 0 to (length teams)-1 do
        let odds_to_advance = ref 0.0 in

        
        let teams_ =
          if t < ((length teams) / 2) then slice teams ((length teams)/2) ((length teams)-1) else slice teams 0 (((length teams)/2)-1) in

        for t_ = 0 to (length teams_)-1 do
          if nth teams t != nth teams_ t_ then
            begin
              if (nth rounds r) = 2 then
                begin
                  let od = odds (nth teams t) (nth teams_ t_) in
                  odds_to_advance := !odds_to_advance +. od
                end
              else
                begin
                  let od = odds (nth teams t) (nth teams_ t_)
                  and prev = round_odds.(r-1).(index tourney (nth teams_ t_) ) in
                  odds_to_advance := !odds_to_advance +. od *. prev
                end
            end
          else ()
        done

        if (nth rounds r) > 2 then
          begin
            let prev = round_odds.(r-1).(index tourney (nth teams t)) in
            odds_to_advance := !odds_to_advance *. prev
          end
        else()
        
        round_odds.(r).((i*(nth rounds r))+t) <- !odds_to_advance

      done
    done
  done
  round_odds.(3).(team_ind);;
 


printf "The odds of two winning right now are %f" (seed_odds tourney 2.);;

