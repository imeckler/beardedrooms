module type Protocol = sig
  type FromServer
  type ToServer

  (* Feel free not to write this function *)
  val interpret : ToServer -> Task Empty FromServer
end

val interpret

module type Client = sig
  type UserUpdate = ...

  type Update
    = User UserUpdate
    | Server FromServer

  type State

  (* See Display.elm for example of how to push updates *)
  val userUpdateBox : Mailbox UserUpdate
  val fromServerBox : Mailbox FromServer

  val update : Update -> State -> (State, List ToServer)

  (* Updates come from user actions *)
  val updates : Signal Update
  updates =
    Signal.mergeMany
    [ Signal.map User userUpdateBox.signal
    , Signal.map Server fromServerBox.signal
    ]

  val stateAndMessagesToServer : Signal (State, List ToServer)

  port outgoing : Signal (Task Empty ())
  port outgoing =
    let
      interpret msg =
        case msg of
          ToServer m ->
            Protocol.interpret m `Task.andThen` Signal.send fromServerBox.address

          ToJavascript jsmsg -> 
            Signal.send toJavascriptBox.address (ToJavascript.toJson jsmsg)
    in
    Signal.map
      (Task.map (\_ -> ()) << Task.sequence << List.map interpret)
      outgoingMessages
  
  module Display = sig
    (* Feel free not to write this function *)
    val display : State -> Html
  end
end

module type Server = sig
  (* ... *)
end
