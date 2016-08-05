{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.Book where

import Opaleye
import Control.Monad (mzero)
import Data.Aeson
import Data.DateTime (DateTime)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import App

data Book' id title body email time = Book
                                        { bpId         :: id
                                        , bpTitle      :: title
                                        , bpBody       :: body
                                        , bpUsersEmail :: email
                                        , bpTimestamp  :: time
                                        }

type BookRead  = Book' BookID String String Email DateTime
type BookWrite = Book' (Maybe BookID) String String Email (Maybe DateTime)

type BPColumnRead = Book' (Column PGInt8)
                          (Column PGText)
                          (Column PGText)
                          (Column PGText)
                          (Column PGTimestamptz)

type BPColumnWrite = Book' (Maybe (Column PGInt8))
                                  (Column PGText)
                                  (Column PGText)
                                  (Column PGText)
                           (Maybe (Column PGTimestamptz))

instance ToJSON BookRead where
  toJSON post = object [ "id"        .= bpId post
                       , "title"     .= bpTitle post
                       , "body"      .= bpBody post
                       , "email"     .= bpUsersEmail post
                       , "timestamp" .= bpTimestamp post
                       ]

instance FromJSON BookWrite where
  parseJSON (Object o) = Book <$>
                                  o .:? "id"    <*>
                                  o .:  "title" <*>
                                  o .:  "body"  <*>
                                  o .:  "email" <*>
                                  o .:? "timestamp"
  parseJSON _ = mzero

$(makeAdaptorAndInstance "pBook" ''Book')

bookPostTable :: Table BPColumnWrite BPColumnRead
bookPostTable = Table "posts" (pBook Book { bpId         = optional "id"
                                          , bpTitle      = required "title"
                                          , bpBody       = required "body"
                                          , bpUsersEmail = required "users_email"
                                          , bpTimestamp  = optional "timestamp"
                                          })

bookPostToPG :: BookWrite -> BPColumnWrite
bookPostToPG = pBook Book { bpId         = const Nothing
                          , bpTitle      = pgString
                          , bpBody       = pgString
                          , bpUsersEmail = pgString
                          , bpTimestamp  = const Nothing
                          }
