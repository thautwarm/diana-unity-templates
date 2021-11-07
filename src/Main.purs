module Main where
import Diana

fact :: Int -> Int
fact x = fact_tco x 1

fact_tco :: Int -> Int -> Int
fact_tco x n
    | x == 0 = n
    | true = fact_tco (x-1) (n * x)

main :: Unit -> Unit
main _ =
    let _ = log "🍝" in 
    let _ = log "555" in
    let k = if 1 > 2 then 5 else 3 in
    log $ fact 10
