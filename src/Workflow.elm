module Workflow exposing (..)
import Browser
import Node as Node
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
import Json.Decode as Decode
import List.Extra as List
import Draggable as Draggable



------------------------ MODEL

type alias Model = { documentId : String, nodes : List Node.Model }

encodeModel : Model -> Encode.Value
encodeModel model = Encode.object [("documentId", Encode.string model.documentId), ("nodes", Encode.list Node.encodeModel model.nodes)]



modelDecoder : Decode.Decoder Model
modelDecoder = Decode.map2 (\documentId nodes -> {documentId=documentId,nodes=nodes})
                    (Decode.field "documentId" Decode.string)
                    (Decode.field "nodes" (Decode.list (Node.modelDecoder)))

------------------------ UDPATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    NoOp -> (model, Cmd.none)

    NodeMsg id subMsg -> 
        let newNodes = List.map (\x -> if x.id == id then Node.update subMsg x else (x, Cmd.none)) model.nodes
            msgs = List.map (\(v,m) -> Cmd.map (NodeMsg v.id) m) newNodes
            values = List.map (Tuple.first) newNodes
        in ({model | nodes = values}, Cmd.batch msgs)


type Msg = NoOp
         | NodeMsg String Node.Msg



---------------------- VIEW


view : Model -> Element.Element Msg
view model = Element.column [] (List.map (\n -> Element.map (NodeMsg n.id) (Node.view n)) model.nodes)


---------------------- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch <| List.map (\n -> Sub.map (NodeMsg n.id) <| Node.subscriptions n) model.nodes