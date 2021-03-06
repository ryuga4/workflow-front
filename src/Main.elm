module Main exposing (..)
import Browser
import Workflow as Workflow
import Node as Node
import Json.Decode as Decode
import Json.Encode as Encode
import Element as Element
import Element.Input as Input
import Element.Background as Background
import Debug as Debug
import Html as Html
import Random as Random
main = Browser.element {init=init,update=update,subscriptions=subscriptions,view=view}


type alias Model = {workflow : Workflow.Model, availableNodes : List (String, String -> Node.Model)}

type Msg 
        = NoOp 
        | WorkflowMsg Workflow.Msg
        | RequestGuidForNode (String -> Node.Model)
        | AddNode Node.Model


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({workflow} as model) = case msg of
    NoOp -> (model, Cmd.none)
    
    WorkflowMsg subMsg -> 
        let (newWorkflow, cmd) = Workflow.update subMsg model.workflow
        in ({model | workflow = newWorkflow}, Cmd.map WorkflowMsg cmd)
    RequestGuidForNode constructor ->
      (model, Random.generate (\n -> AddNode (constructor <| String.fromInt n)) (Random.int Random.minInt Random.maxInt))
    AddNode node -> 
      let newWorkflow = {workflow | nodes = workflow.nodes ++ [node]}
      in ({model | workflow = newWorkflow}, Cmd.none)
view : Model -> Html.Html Msg
view model = Element.layout [] <| Element.row []
    [ Element.column [] <| List.map (\(name, constructor) -> 
          Input.button [Background.color (Element.rgb255 238 238 238), Element.focused [Background.color (Element.rgb255 150 150 200)]] {onPress = Just (RequestGuidForNode constructor), label = Element.text name} 
        ) model.availableNodes
    , Element.el [Element.alignTop] (Element.map WorkflowMsg (Workflow.view model.workflow))
    , Input.multiline [] {onChange = \_ -> NoOp, text = Encode.encode 5 (Workflow.encodeModel model.workflow), placeholder = Nothing, label = Input.labelHidden "kod", spellcheck = False}
    ]
  

subscriptions : Model -> Sub Msg
subscriptions model = Sub.map WorkflowMsg (Workflow.subscriptions model.workflow)






init : () -> (Model, Cmd Msg)
init () = 
    case Decode.decodeString (Decode.list Node.modelDecoder) jsonString of
        Ok nodes -> ({workflow = { documentId = "testDocumentId", nodes = []}, availableNodes = nodes}, Cmd.none)
        Err e -> Debug.log (Decode.errorToString e) ({workflow = { documentId = "testDocumentId", nodes = []}, availableNodes = []}, Cmd.none)



jsonString : String
jsonString = """
[
  {
    "nodeType": "WorkflowRabbit.WorkflowNodes.DocumentSignedNotifyNode.DocumentSignedNotifyNodeDefinition`1, WorkflowRabbit, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null",
    "nodeName": "Document signed notify node",
    "outputs": [],
    "nodeDefinition": {
      "email": {
        "type": "text",
        "label": "Email"
      },
      "nextNodeID": {
        "type": "nodeID",
        "label": "Next node"
      }
    }
  },
  {
    "nodeType": "WorkflowRabbit.WorkflowNodes.FinishNode.FinishNodeDefinition`1, WorkflowRabbit, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null",
    "nodeName": "Finish Node",
    "outputs": [],
    "nodeDefinition": {
      "funnyNumbers": {
        "subInput": {
          "type": "number",
          "label": "Podaj liczbe"
        },
        "min": 0,
        "max": 2147483647,
        "type": "list",
        "label": "Funny numbers"
      }
    }
  },
  {
    "nodeType": "WorkflowRabbit.WorkflowNodes.RaceFinishNode.RaceFinishNodeDefinition`1, WorkflowRabbit, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null",
    "nodeName": "Race finish node",
    "outputs": [],
    "nodeDefinition": {
      "nextNodeID": {
        "type": "nodeID",
        "label": "Next node"
      }
    }
  },
  {
    "nodeType": "WorkflowRabbit.WorkflowNodes.SendToSignNode.SendToSignNodeDefinition`1, WorkflowRabbit, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null",
    "nodeName": "Send to sign node",
    "outputs": [],
    "nodeDefinition": {
      "email": {
        "type": "text",
        "label": "Email"
      },
      "nextNodeID": {
        "type": "nodeID",
        "label": "Next node"
      }
    }
  },
  {
    "nodeType": "WorkflowRabbit.WorkflowNodes.StraightPassNode.StraightPassNodeDefinition`1, WorkflowRabbit, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null",
    "nodeName": "Straight pass node",
    "outputs": [],
    "nodeDefinition": {
      "message": {
        "type": "text",
        "label": "Message"
      },
      "nextNodeID": {
        "type": "nodeID",
        "label": "Next node"
      }
    }
  },
  {
    "nodeType": "WorkflowRabbit.WorkflowNodes.RaceNode.RaceNodeDefinition`1, WorkflowRabbit, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null",
    "nodeName": "Race node",
    "outputs": [],
    "nodeDefinition": {
      "successors": {
        "subInput": {
          "type": "nodeID",
          "label": "Successor"
        },
        "min": 0,
        "max": 2147483647,
        "type": "list",
        "label": "Successors"
      }
    }
  },
  {
    "nodeType": "WorkflowRabbit.WorkflowNodes.WaitForSignNode.WaitForSignNodeDefinition`1, WorkflowRabbit, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null",
    "nodeName": "Straight pass node",
    "outputs": [],
    "nodeDefinition": {
      "signatureType": {
        "type": "text",
        "label": "SignatureType"
      },
      "nextNodeID": {
        "type": "nodeID",
        "label": "Next node"
      }
    }
  },
  {
    "nodeType": "WorkflowRabbit.WorkflowNodes.WaitNode.WaitNodeDefinition`1, WorkflowRabbit, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null",
    "nodeName": "Wait node",
    "outputs": [],
    "nodeDefinition": {
      "timeSpanString": {
        "type": "text",
        "label": "Czas w formacie HH:MM:SS"
      },
      "nextNodeID": {
        "type": "nodeID",
        "label": "Next Node"
      }
    }
  }
]
"""