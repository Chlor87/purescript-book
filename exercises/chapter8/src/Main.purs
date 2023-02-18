module Main where

import Prelude

import Data.AddressBook (PhoneNumber, examplePerson)
import Data.AddressBook.Validation (Errors, Field(..), ValidationError(..), validatePerson')
import Data.Array (filter, length, mapWithIndex, updateAt)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, Component, element, reactComponent, useState, (/\))
import React.Basic.Hooks as R
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- Note that there's a Purty formatting bug that
-- adds an unwanted blank line
-- https://gitlab.com/joneshf/purty/issues/77
-- ANCHOR: renderValidationErrors
renderValidationErrors :: Errors -> Array R.JSX
renderValidationErrors [] = []
renderValidationErrors xs =
  let
    renderError :: ValidationError -> R.JSX
    renderError (ValidationError _ err) = D.div
      { className:
          "alert alert-danger"
      , children: [ D.text err ]
      }
  in
    renderError <$> xs

-- ANCHOR_END: renderValidationErrors

-- Helper function to render a single form field with an
-- event handler to update
-- ANCHOR: formField
formField :: Field -> Errors -> String -> (String -> Effect Unit) -> R.JSX
formField field errors value setValue =
  let
    e = filter (\(ValidationError f _) -> f == field) errors
    error = case e of
      [ (ValidationError _ e) ] -> D.div
        { className: "invalid-feedback"
        , children: [ D.text e ]
        }
      _ -> R.empty
    hasError = length e == 1

  in
    D.label
      { className: "form-group row"
      , children:
          [ D.div
              { className: "col-sm-4 col-form-label"
              , children: [ D.text $ show field ]
              }
          , D.div
              { className: "col-sm-8"
              , children:
                  [ D.input
                      { className: "form-control " <> if hasError then "is-invalid" else "is-valid"
                      , placeholder: show field
                      , value
                      , onChange:
                          let
                            handleValue :: Maybe String -> Effect Unit
                            handleValue (Just v) = setValue v
                            handleValue Nothing = pure unit
                          in
                            handler targetValue handleValue
                      }
                  , error
                  ]
              }
          ]
      }

-- ANCHOR_END: formField

mkAddressBookApp :: Effect (ReactComponent {})
mkAddressBookApp =
  -- incoming \props are unused
  reactComponent "AddressBookApp" \_ -> R.do
    -- `useState` takes a default initial value and returns the
    -- current value and a way to update the value.
    -- Consult react-hooks docs for a more detailed explanation of `useState`.
    person /\ setPerson <- useState examplePerson
    let
      errors = case validatePerson' person of
        Left e -> e
        Right _ -> []

      -- helper-function to return array unchanged instead of Nothing if index is out of bounds
      updateAt' :: forall a. Int -> a -> Array a -> Array a
      updateAt' i x xs = fromMaybe xs (updateAt i x xs)

      -- helper-function to render a single phone number at a given index
      renderPhoneNumber :: Int -> PhoneNumber -> R.JSX
      renderPhoneNumber index phone =
        formField
          (Phone phone.type)
          errors
          phone.number
          (\s -> setPerson _ { phones = updateAt' index phone { number = s } person.phones })

      -- helper-function to render all phone numbers
      renderPhoneNumbers :: Array R.JSX
      renderPhoneNumbers = mapWithIndex renderPhoneNumber person.phones
    -- ANCHOR: mkAddressBookApp_pure
    pure
      $ D.div
          { className: "container"
          , children:
              -- renderValidationErrors errors
              --   <>
              [ D.div
                  { className: "row"
                  , children:
                      [ D.div
                          { className: "col-sm-12"
                          , children:
                              [ D.form_
                                  $
                                    [ D.h3_ [ D.text "Basic Information" ]
                                    , formField FirstName errors person.firstName \s ->
                                        setPerson _ { firstName = s }
                                    , formField LastName errors person.lastName \s ->
                                        setPerson _ { lastName = s }
                                    , D.h3_ [ D.text "Address" ]
                                    , formField Street errors person.homeAddress.street \s ->
                                        setPerson _ { homeAddress { street = s } }
                                    , formField City errors person.homeAddress.city \s ->
                                        setPerson _ { homeAddress { city = s } }
                                    , formField State errors person.homeAddress.state \s ->
                                        setPerson _ { homeAddress { state = s } }
                                    , D.h3_ [ D.text "Contact Information" ]
                                    ]
                                      <> renderPhoneNumbers
                              ]
                          }
                      ]
                  }
              ]
          }

-- ANCHOR_END: mkAddressBookApp_pure

-- ANCHOR: main
main :: Effect Unit
main = unsafePartial do
  log "Rendering address book component"
  Just ctr <- getElementById "container" <<< toNonElementParentNode =<< document =<< window
  addressBookApp <- mkAddressBookApp
  D.render (element addressBookApp {}) ctr
