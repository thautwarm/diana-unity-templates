module Main where
import Diana

discard :: Unit -> Unit
discard _ = unit

ignore :: forall a. a -> Unit
ignore _ = unit

main :: Unit -> Unit
main _ =
    let _ = ignore (log "🍝") in
    log $ 2 + 2.0
