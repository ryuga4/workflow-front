module Node exposing (..)
import Browser
import Html as Html
import Html.Events as Events
import Html.Attributes as Attributes
import Dict exposing (..)
import Element as Element
import Element.Input as Input
import Element.Border as Border
import List
import Element.Background as Background
import Json.Encode as Encode
import Json.Decode as Decode
import List.Extra as List
import Draggable as Draggable



-------------- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Draggable.subscriptions DragMsg (model.drag)




--------------- TYPES

type Index = TextIndex String | NumberIndex Int

type Msg 
        = NoOp
        | TextChanged String
        | NodeIDChanged String
        | NumberChanged Int
        | NestedMsg Index Msg
        | AddElement
        | DeleteElement Int
        | DragMsg (Draggable.Msg String)
        | OnDragBy Draggable.Delta
        | OutputKeyChanged String
        

type alias Model = NodeInstanceViewModel


type alias NodeInstanceViewModel = 
  { nodeType : String
  , id : String
  , nodeName : String
  , nodeDefinition : NodeInput String Int String
  , position : (Int,Int)
  , drag : Draggable.State String
  }


type NodeInput t n nid = Object (Dict String (NodeInput t n nid))
               | TextInput t
               | NumberInput n
               | NodeIDInput nid
               | ListInput (NodeInput t n nid) (List (NodeInput t n nid)) 
               | NodeOutput String



type alias NodeDescription = 
  { nodeType : String
  , outputs : List ()
  , nodeDefinition : NodeInput () () ()
  , position : (Int,Int)
  , drag : Draggable.State String
  }



dragConfig : Draggable.Config String Msg
dragConfig =
    Draggable.basicConfig OnDragBy




-------------------- view




view : Model -> Element.Element Msg
view model =
  Element.row [Element.padding 10] 
        [ (nodeToView model)
        ]


nodeToView : NodeInstanceViewModel -> Element.Element Msg
nodeToView node = Element.column [Background.color (Element.rgb255 250 250 250),Element.alignTop, Border.width 1, Element.htmlAttribute (Attributes.style "z-index" "100"), Element.htmlAttribute (Attributes.style "position" "absolute"), Element.htmlAttribute (Attributes.style "left" (String.fromInt (Tuple.first node.position) ++ "px")), Element.htmlAttribute (Attributes.style "top" (String.fromInt (Tuple.second node.position) ++ "px"))]
  [ Element.text node.id
  , Element.row [] [Element.el [Element.htmlAttribute (Draggable.mouseTrigger node.id DragMsg)] (Element.text "grab me  "), Element.el [Element.alignRight] <| Element.text node.nodeName]
  , Element.text (let (x,y) = (node.position) in "(" ++ String.fromInt x ++ ", "++ String.fromInt y ++ ")")
  , nodeInputToView 1 node.nodeDefinition
  ]

nodeInputToView : Int -> NodeInput String Int String -> Element.Element Msg
nodeInputToView lvl input = case input of

  (Object dict) -> 
    Element.row [Element.padding 4] 
    <| List.map (\(k, v) -> Element.column [Element.padding 10, Border.width lvl] [Element.text k, Element.map (\m -> NestedMsg (TextIndex k) m) <| nodeInputToView (lvl+1) v])
    <| Dict.toList dict

  (TextInput t) -> 
    Input.multiline [Element.padding 4, Element.width (Element.fill |> Element.minimum 100 |> Element.maximum 150)]
      { onChange = \t2 -> TextChanged t2 
      , text = t
      , placeholder = Nothing
      , label = Input.labelHidden "podaj tekst"
      , spellcheck = False
      }

  (NodeIDInput nodeID) ->
    Input.multiline [Element.padding 4, Element.width (Element.fill |> Element.minimum 100 |> Element.maximum 150)]
      { onChange = \t2 -> NodeIDChanged t2 
      , text = nodeID
      , placeholder = Nothing
      , label = Input.labelHidden "podaj tekst"
      , spellcheck = False
      }

  (NumberInput n) -> 
     Input.multiline [Element.padding 4, Element.width (Element.fill |> Element.minimum 100 |> Element.maximum 150)]
      { onChange = (NumberChanged << Maybe.withDefault n << String.toInt) 
      , text = String.fromInt n
      , placeholder = Nothing
      , label = Input.labelHidden "podaj tekst"
      , spellcheck = False
      }


  (ListInput sampleSubInput subInputs) ->
    Element.column [Element.padding 4] <|
       (Input.button [Background.color (Element.rgb255 238 238 238), Element.focused [Background.color (Element.rgb255 150 150 200)]] {onPress = Just AddElement, label = Element.text "Add element"} 
        :: (List.indexedMap (\i v -> 
            Element.row []
            [ Element.map (\m -> NestedMsg (NumberIndex i) m) v
            ,  Input.button [Background.color (Element.rgb 200 0 0)] {onPress = Just (DeleteElement i), label = Element.text ("delete item")}
            ]) <| List.map (nodeInputToView (lvl+1)) subInputs))

  (NodeOutput outputKey) ->
    Input.multiline [Element.padding 4, Element.width (Element.fill |> Element.minimum 100 |> Element.maximum 150)]
      { onChange = \t2 -> OutputKeyChanged t2 
      , text = outputKey
      , placeholder = Nothing
      , label = Input.labelHidden "podaj outputKey"
      , spellcheck = False
      }


-------------------------- UPDATE




update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of

  (OnDragBy (dx,dy)) ->
    let (x,y) = model.position
    in ({model | position = (round (toFloat x + dx), round (toFloat y + dy))}, Cmd.none)

  (DragMsg dragMsg) ->
    let (newViewModel,cmd) = Draggable.update dragConfig dragMsg model
    in (newViewModel, cmd)

  _ ->  
    let (newNodeDefinition, cmd) = updateInput msg model.nodeDefinition
    in ({model | nodeDefinition = newNodeDefinition}, cmd)

updateInput : Msg -> NodeInput String Int String -> (NodeInput String Int String, Cmd Msg)
updateInput msg nodeInput = case (msg, nodeInput) of
  (NoOp, _) -> (nodeInput, Cmd.none)

  (TextChanged t, TextInput _) -> (TextInput t, Cmd.none)
  (NodeIDChanged n, NodeIDInput _) -> (NodeIDInput n, Cmd.none)
  (NumberChanged n, NumberInput _) -> (NumberInput n, Cmd.none)
  (NestedMsg (TextIndex textKey) subMsg, Object dict) -> 
        let updatedSubInputs = Dict.map (\k v -> if k == textKey then updateInput subMsg v else (v, Cmd.none)) dict
            cmds = List.map (\(_,cmd) -> cmd) <| Dict.values updatedSubInputs
            values = Dict.map (\_ (v,_) -> v) <| updatedSubInputs
        in
        (Object values , Cmd.batch cmds)

  (NestedMsg (NumberIndex numberKey) subMsg, ListInput sampleInput subInputs) -> 
      let updatedSubInputs = List.indexedMap (\i v -> if i == numberKey then updateInput subMsg v else (v, Cmd.none)) subInputs
          cmds = List.map (\(_,cmd) -> cmd) updatedSubInputs
          values = List.map (\(v,_)->v) updatedSubInputs
      in (ListInput sampleInput values, Cmd.batch cmds)

  (AddElement, ListInput sampleInput subInputs) -> (ListInput sampleInput (subInputs ++ [sampleInput]), Cmd.none)

  (DeleteElement ix, ListInput sampleInput subInputs) -> (ListInput sampleInput <| List.removeAt ix subInputs, Cmd.none)

  (OutputKeyChanged k, NodeOutput _) -> (NodeOutput k, Cmd.none)
  _ -> (nodeInput, Cmd.none)


----------------------------- JSON



encodeModel : Model -> Encode.Value
encodeModel model = Encode.object
  [ ("nodeType", Encode.string model.nodeType)
  , ("id", Encode.string model.id)
  , ("nodeDefinition", encodeNodeInput model.nodeDefinition)
  ]

encodeNodeInput : NodeInput String Int String -> Encode.Value
encodeNodeInput nodeInput = case nodeInput of
  Object dict -> Encode.object <| List.map (\(k,v) -> (k,encodeNodeInput v)) <| Dict.toList dict

  TextInput t -> Encode.object [("inputMethod", Encode.string "basic"), ("value", Encode.string t)]

  NumberInput n -> Encode.object [("inputMethod", Encode.string "basic"), ("value", Encode.int n)]

  ListInput sampleInput subInputs -> Encode.object [("inputMethod", Encode.string "list"),("inputs", Encode.list encodeNodeInput subInputs)]

  NodeIDInput s -> Encode.object [("inputMethod", Encode.string "basic"), ("value", Encode.string s)]

  NodeOutput k -> Encode.object [("inputMethod", Encode.string "output"), ("key", Encode.string k)]


nodeInputDecoder : Decode.Decoder (NodeInput String Int String)
nodeInputDecoder = Decode.oneOf 
                    [ Decode.map5 (\inputType inputLabel maybeMin maybeMax maybeSubInput -> case ((inputType, inputLabel),(maybeMin, maybeMax, maybeSubInput)) of 
                                                              (("text", l), (Nothing, Nothing, Nothing)) -> TextInput ""
                                                              (("number", l), (Nothing, Nothing, Nothing)) -> NumberInput 0
                                                              (("nodeID", l), (Nothing, Nothing, Nothing)) -> NodeIDInput ""
                                                              (("list", l), (Just min, Just max, Just subInput)) -> ListInput subInput []
                                                              (("output", l), (Nothing,Nothing,Nothing)) -> NodeOutput ""
                                                              _ -> TextInput "ERROR"
                                                              ) 
                                                              (Decode.field "type" Decode.string)
                                                              (Decode.field "label" Decode.string)
                                                              (Decode.maybe (Decode.field "min"  Decode.int))
                                                              (Decode.maybe (Decode.field "max" Decode.int))
                                                              (Decode.maybe (Decode.field "subInput" <| Decode.lazy (\() -> nodeInputDecoder)))
                    , Decode.map Object (Decode.dict (Decode.lazy (\() -> nodeInputDecoder)))
                    ]

----------------------------- HELPERS






modelDecoder : Decode.Decoder (String, String -> Model)
modelDecoder = Decode.map3 (\nodeType nodeName nodeDefinition -> (nodeName, \id -> {nodeType = nodeType, nodeName=nodeName, id = id, nodeDefinition = nodeDefinition, position = (0,0), drag = Draggable.init}))
                  (Decode.field "nodeType" Decode.string)
                  (Decode.field "nodeName" Decode.string)
                  (Decode.field "nodeDefinition" nodeInputDecoder)