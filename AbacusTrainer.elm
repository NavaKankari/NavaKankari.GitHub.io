import Html exposing ( div, Html )
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Color exposing (..)
import Random exposing (..)
--import StartApp.Simple exposing( start )

renderGUI : Signal.Address Action  -> Model -> Html.Html
renderGUI picture_address current_state =
    svg
      [ width "2400", height "240", viewBox "0 0 2400 240", fill "blue"]
      ( List.concat [ 
        (ten_circles picture_address)
      , [ ( rect [ x "0", y "120",   width "120", height "120", rx "15", ry "15",  fill "lightBlue", Svg.Events.onClick ( Signal.message picture_address ( ClickRed ) ) ] [] ) ]
      ]
      )


--//         ( rect [ x "0", y "0",   width "1200", height "120", rx "15", ry "15",  fill "lightBlue"  ] [] ),
--        ( rect [ x "0", y "120", width "1200", height "120", rx "15", ry "15",  fill "lightGreen" ] [] ),
--        ( circle [ cx "60", cy "60", r "30", fill "brown",   Svg.Events.onClick ( Signal.message picture_address ClickRed   ) ] 
--           [
--            
--           ] 
--        ),
--        ( circle [ cx "10", cy "10", r "10", fill "green", Svg.Events.onClick ( Signal.message picture_address ClickGreen ) ] [] )
        
        
--main =
--  svg
--      [ width "1200", height "240", viewBox "0 0 1200 240", fill "blue"]
--      [
--        ( rect [ x "0", y "0",   width "1200", height "120", rx "15", ry "15",  fill "lightBlue"  ] [ text' [] [ text ( toString 24 )]  ]  ),
--        ( line [ x1 "600", y1 "0", x2 "600", y2 "120",  Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2" ] [] )
--      ]


score current_state =
  text' [ x "10", y "110", fill "blue" ] [ text ( toString current_state ) ] 

cr30 x y n picture_address =
    circle 
     [ 
      cx x, cy y, r "30", fill "white", 
      Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2", 
      Svg.Events.onClick ( Signal.message picture_address ( ClickedBall n ) )  -- ( ClickGreen ) ) -- 
     ] [ text' [] [ text ( toString n)] ] 
    

ten_circles : Signal.Address Action  -> List Svg    
ten_circles picture_address =
    let cx n =
       cr30 ( ( n * 60 + 1000 )|> toString ) "60" n picture_address
    in 
      List.map cx [0..9]

justGUI_main = renderGUI picture.address initial_state

live_main: Signal Html
live_main =  Signal.map ( view picture.address ) ( updatep picture.signal )

main = 
    live_main
    --justGUI_main


type alias Model = 
  { 
      x:Int, y:Int
    , rng  : Generator Int
    , target_number_and_seed : ( Int, Seed )
  }


initial_state: Model
initial_state = { x=0, y=0, rng = int 0 10, target_number_and_seed = ( 0,  initialSeed 314 ) }

type Action = ClickRed | ClickGreen | Reset | ClickedBall Int

picture : Signal.Mailbox Action 
picture = Signal.mailbox ClickGreen

updatep : Signal Action -> Signal Model
updatep actions  = Signal.foldp update initial_state actions

update : Action -> Model -> Model
update action current_state =
 case action of
  ClickRed   -> 
    { current_state | target_number_and_seed = generate current_state.rng ( snd current_state.target_number_and_seed )  }
  ClickGreen -> 
    { current_state | y = current_state.y +1 }
  Reset      -> 
    { current_state | x=13, y=17 }
  ClickedBall index ->
    { current_state |x = index, y = ( fst current_state.target_number_and_seed ) }


view : Signal.Address Action -> Model -> Html
view address current_state =
  div []
  [
     div [] [ renderGUI picture.address current_state  ]
   , div[ Html.Events.onClick address Reset ] [ Html.text (toString current_state ) ]
  ]
