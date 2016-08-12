{-# LANGUAGE Arrows #-}
module Queries.BlogPost where

import Opaleye
import Control.Arrow (returnA)

import App
import Models.BlogPost

blogPostsQuery :: Query BPColumnRead
blogPostsQuery = queryTable blogPostTable

blogPostByIdQuery :: BlogPostID -> Query BPColumnRead
blogPostByIdQuery idToMatch = proc () -> do
                         post     <- blogPostsQuery -< ()
                         restrict -< bpId post .== pgInt8 idToMatch
                         returnA  -< post

blogPostsByEmailQuery :: Email -> Query BPColumnRead
blogPostsByEmailQuery email = proc () -> do
                                post     <- blogPostsQuery -< ()
                                restrict -< bpUsersEmail post .== pgString email
                                returnA  -< post
