module Main where

import Prelude
import DOM.HTML as DOM
import DOM.HTML.Types as DOM
import DOM.HTML.Window as DOM
import DOM.Node.ParentNode as DOM
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import Data.Array (cons, concat, filter)
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)
import Unsafe.Coerce (unsafeCoerce)

data Action = NewItem | SetNewText String | Remove Int

type ToDoItem = { name :: String}
type SavedToDoItem = {name :: String, id :: Int}
type State = { newItem :: ToDoItem, items :: Array SavedToDoItem }

initialState :: State
initialState = { newItem: {name: ""}, items: [] }

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value

render :: T.Render State _ Action
render dispatch _ state _ =
  let itemsUI = map (\i ->
    R.p' [R.text i.name, R.button [ RP.onClick \_ -> dispatch (Remove i.id)  ] [R.text "Done"]]) state.items in
  concat
  [
    [ R.p' [ R.strong' [R.text "Matt's todo app!"]]
    , R.p' [ R.text "Items that already exist:"]
    ],
    itemsUI,
    [R.p' [ R.text "New Item:"
           , R.input [ RP.onChange \event -> dispatch (SetNewText (unsafeEventValue event))
                     , RP.value state.newItem.name ] []]
    , R.p' [ R.button [ RP.onClick \_ -> dispatch NewItem ]
                      [ R.text "Save"]
           ]
    ]
  ]

getMax :: Array SavedToDoItem -> Int
getMax x = Data.Array.length x

performAction :: T.PerformAction _ State _ Action
performAction NewItem _ _ update = update $ \state ->
      let nextId = getMax state.items in
      let newItem = {name: state.newItem.name, id: nextId} in
      let updatedItems = (cons newItem state.items) in
      state { newItem = {name: ""}, items = updatedItems}
performAction (SetNewText text) _ _ update = update $ \state -> state {newItem = {name: text}}
performAction (Remove time) _ _ update = update $ \state -> state {items = filter (\i -> i.id /= time) state.items}

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

main = do
  let component = T.createClass spec initialState
  document <- DOM.window >>= DOM.document
  container <- fromJust <<< toMaybe <$> DOM.querySelector "#container" (DOM.htmlDocumentToParentNode document)
  RDOM.render (R.createFactory component {}) container
