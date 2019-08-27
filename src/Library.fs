module FSharpPlus.Logic
open FSharpPlus.Data
[<Struct>]
type LogicT<'m_r,'a> = {
  unLogicT : ('a -> 'm_r -> 'm_r) -> 'm_r -> 'm_r
}
with
  static member Return x = { unLogicT=fun sk fk -> sk x fk }
  static member (>>=) ({unLogicT=m}, f) = 
    let unLogicT {unLogicT=unLogicT}=unLogicT
    { unLogicT= (fun sk fk -> m (fun a fk' -> unLogicT (f a) sk fk' ) fk) }
  static member (<*>) ({unLogicT=f}, {unLogicT=x}) = { unLogicT= fun sk fk -> f (fun g fk' -> x (sk << g) fk' ) fk}
  static member Map ({unLogicT=unLogicT}, f : 'T->'U) = { unLogicT=fun sk fk -> unLogicT (sk << f) fk }

  static member inline Lift (m: '``Monad<'T>``) = { unLogicT= ((>>=) m)} : LogicT<'``Monad<'R>``,'T>    

  static member inline get_Empty () = {unLogicT=fun _ fk -> fk}
  static member inline (<|>) ({unLogicT=f1}, {unLogicT=f2}) = {unLogicT= fun sk fk -> f1 sk (f2 sk fk) }


type Logic<'a> = LogicT<Identity<'a>,'a>


/// Basic operations on LogicT
[<RequireQualifiedAccess>]
module LogicT = 
  let run {unLogicT=unLogicT}=unLogicT
  let inline observe lt = run lt (konst << result) (failwith "No answer.")
  let inline observeAll m = run m (map << List.cons) (result [])
/// Basic operations on Logic
[<RequireQualifiedAccess>]
module Logic = 
  let run l s f=
    let si = map << s
    let fi = Identity f
    Identity.run <| LogicT.run l si fi
