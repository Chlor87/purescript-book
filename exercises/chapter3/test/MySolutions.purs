module Test.MySolutions where

import Prelude

import Data.AddressBook (AddressBook, Entry)
import Data.Function (on)
import Data.List (find, nubByEq)
import Data.Maybe (Maybe, isJust)
import Unsafe.Coerce (unsafeCoerce)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet s = find (eq s <<< _.address.street)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook f l = isJust <<< find \{ firstName: f', lastName: l' } -> f' == f && l' == l

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq $ eq `on` (unsafeCoerce :: Entry -> { firstName :: String, lastName :: String })
