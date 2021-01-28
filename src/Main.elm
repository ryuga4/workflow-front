module Main exposing (..)
import Browser
import Html exposing (Html, button, div)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Dict exposing (..)
import Element as Element
import Element.Input as Input
import Element.Border as Border
import List
import Element.Background as Background
import Json.Encode as Encode
import List.Extra as List
main =
  Browser.sandbox { init = init, update = update, view = view }


type Index = TextIndex String | NumberIndex Int

type Msg 
        = NoOp
        | TextChanged String
        | NestedMsg Index Msg
        | AddElement
        | DeleteElement Int


type alias Model = { nodeDescription : NodeDescription
                   , viewModel : NodeInstanceViewModel}




viewModelToJson : NodeInstanceViewModel -> Encode.Value
viewModelToJson model = Encode.object
  [ ("NodeType", Encode.string model.nodeType)
  , ("Id", Encode.string model.id)
  , ("NodeDefinition", nodeInputToJson model.nodeDefinition)
  ]

nodeInputToJson : NodeInput String Int -> Encode.Value
nodeInputToJson nodeInput = case nodeInput of
  Object dict -> Encode.object <| List.map (\(k,v) -> (k,nodeInputToJson v)) <| Dict.toList dict

  TextInput t -> Encode.object [("InputMethod", Encode.string "basic"), ("Value", Encode.string t)]

  NumberInput n -> Encode.object [("InputMethod", Encode.string "basic"), ("Value", Encode.int n)]

  ListInput sampleInput subInputs -> Encode.object [("InputMethod", Encode.string "list"),("Inputs", Encode.list nodeInputToJson subInputs)]

type alias NodeInstanceViewModel = 
  { nodeType : String
  , id : String
  , nodeDefinition : NodeInput String Int
  }
type NodeInput t n = Object (Dict String (NodeInput t n))
               | TextInput t
               | NumberInput n
               | ListInput (NodeInput t n) (List (NodeInput t n)) 
init = {viewModel = descriptionToViewModel exampleDescription, nodeDescription = exampleDescription}

update : Msg -> Model -> Model
update msg model = 
  let viewModel = model.viewModel
  in {model | viewModel = {viewModel | nodeDefinition = updateInput msg viewModel.nodeDefinition}}

updateInput : Msg -> NodeInput String Int -> NodeInput String Int
updateInput msg nodeInput = case (msg, nodeInput) of
  (NoOp, _) -> nodeInput

  (TextChanged t, TextInput _) -> TextInput t

  (NestedMsg (TextIndex textKey) subMsg, Object dict) -> Object <| Dict.map (\k v -> if k == textKey then updateInput subMsg v else v) dict

  (NestedMsg (NumberIndex numberKey) subMsg, ListInput sampleInput subInputs) -> 
    ListInput sampleInput <| List.indexedMap (\i v -> if i == numberKey then updateInput subMsg v else v) subInputs

  (AddElement, ListInput sampleInput subInputs) -> ListInput sampleInput (sampleInput :: subInputs)

  (DeleteElement ix, ListInput sampleInput subInputs) -> ListInput sampleInput <| List.removeAt ix subInputs
  _ -> nodeInput

view : Model -> Html Msg
view model = Element.layout [] <| 
  Element.row [Element.padding 10] 
        [ Input.multiline []
          { onChange = \_ -> NoOp
          , text = (Encode.encode 5 (viewModelToJson model.viewModel))
          , placeholder = Nothing
          , label = Input.labelHidden "kod do zwrotki na serwer"
          , spellcheck = False
          }
        , (nodeToView model.viewModel)
        ]

type alias NodeDescription = 
  { nodeType : String
  , outputs : List ()
  , nodeDefinition : NodeInput () ()
  }



nodeToView : NodeInstanceViewModel -> Element.Element Msg
nodeToView node = Element.column [Element.alignTop]
  [ nodeInputToView 1 node.nodeDefinition
  ]

nodeInputToView : Int -> NodeInput String Int -> Element.Element Msg
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

  (NumberInput n) ->
    Input.slider [Element.padding 4]
    { onChange = \t2 -> NoOp
    , label = Input.labelHidden "podaj liczbe"
    , min = 0
    , max = 10
    , value = toFloat n
    , thumb = Input.thumb []
    , step = Just 1
    }

  (ListInput sampleSubInput subInputs) ->
    Element.column [Element.padding 4] <|
       (Input.button [Background.color (Element.rgb255 238 238 238), Element.focused [Background.color (Element.rgb255 150 150 200)]] {onPress = Just AddElement, label = Element.text "Add element"} 
        :: (List.indexedMap (\i v -> 
            Element.row []
            [ Element.map (\m -> NestedMsg (NumberIndex i) m) v
            ,  Input.button [Background.color (Element.rgb 200 0 0)] {onPress = Just (DeleteElement i), label = Element.text ("delete item")}
            ]) <| List.map (nodeInputToView (lvl+1)) subInputs))




mapInput : (t -> t2) -> (n -> n2) -> NodeInput t n -> NodeInput t2 n2
mapInput tf nf input = case input of
  (Object dict) ->  
  
    Object <| Dict.map (\_ v -> mapInput tf nf v) dict
  
  (TextInput t) -> 
    TextInput (tf t)
  
  (NumberInput n) -> 
    NumberInput (nf n)

  (ListInput sampleInput subInputs) ->
    ListInput (mapInput tf nf sampleInput) (List.map (mapInput tf nf) subInputs) 
exampleDescription : NodeDescription
exampleDescription =  
  { nodeType = "WorkflowRabbit.WorkflowNodes.StraightPassNode.StraightPassNodeDefinition`1, WorkflowRabbit, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
  , outputs = []
  , nodeDefinition = Object <| Dict.fromList
    [ ("Message", TextInput ())
    , ("NextNodeID", TextInput ())
    , ("Successors", ListInput (TextInput ()) [])
    ]
  }


descriptionToViewModel : NodeDescription -> NodeInstanceViewModel
descriptionToViewModel node = 
  { nodeType = node.nodeType
  , id = "abc"
  , nodeDefinition = mapInput (\() -> "") (\() -> 0) (node.nodeDefinition)
  }


