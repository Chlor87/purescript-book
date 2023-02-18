module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Address, Person, PhoneNumber, PhoneType(..), address, person, phoneNumber)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (length)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, invalid, toEither)

data Field
  = FirstName
  | LastName
  | Street
  | City
  | State
  | Phone PhoneType

derive instance Generic Field _
derive instance Eq PhoneType => Eq Field

instance Show PhoneType => Show Field where
  show (Phone t) = "Phone " <> show t
  show t = genericShow t

data ValidationError = ValidationError Field String

type Errors = Array ValidationError

nonEmpty :: Field -> String -> V Errors String
nonEmpty field "" = invalid [ ValidationError field $ "Field '" <> show field <> "' cannot be empty" ]
nonEmpty _ value = pure value

validatePhoneNumbers :: Array PhoneNumber -> V Errors (Array PhoneNumber)
validatePhoneNumbers [] =
  invalid [ ValidationError (Phone HomePhone) $ "Field '" <> "Phones" <> "' must contain at least one value" ]
validatePhoneNumbers phones =
  traverse validatePhoneNumber phones

lengthIs :: Field -> Int -> String -> V Errors String
lengthIs field len value | length value /= len =
  invalid [ ValidationError field $ "Field '" <> show field <> "' must have length " <> show len ]
lengthIs _ _ value = pure value

phoneNumberRegex :: Regex
phoneNumberRegex = unsafeRegex "^\\d{3}-\\d{3}-\\d{4}$" noFlags

matches :: Field -> Regex -> String -> V Errors String
matches _ regex value | test regex value = pure value
matches field _ _ = invalid [ ValidationError field $ "Field '" <> show field <> "' did not match the required format" ]

validateAddress :: Address -> V Errors Address
validateAddress a =
  address <$> nonEmpty Street a.street
    <*> nonEmpty City a.city
    <*> lengthIs State 2 a.state

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber pn =
  phoneNumber <$> pure pn."type"
    <*> matches (Phone pn.type) phoneNumberRegex pn.number

validatePerson :: Person -> V Errors Person
validatePerson p =
  person <$> nonEmpty FirstName p.firstName
    <*> nonEmpty LastName p.lastName
    <*> validateAddress p.homeAddress
    <*> validatePhoneNumbers p.phones

validatePerson' :: Person -> Either Errors Person
validatePerson' p = toEither $ validatePerson p
