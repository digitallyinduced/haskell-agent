module Agent.CLI.Lsp.Capabilities
    ( clientCapabilities
    ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Text (Text)

clientCapabilities :: Aeson.Value
clientCapabilities =
    Aeson.object
        [ "workspace" .= Aeson.object
            [ "symbol" .= Aeson.object []
            , "workspaceFolders" .= True
            , "configuration" .= True
            , "applyEdit" .= False
            ]
        , "textDocument" .= Aeson.object
            [ "definition" .= Aeson.object []
            , "references" .= Aeson.object []
            , "hover" .= Aeson.object
                [ "contentFormat" .=
                    [ "markdown" :: Text
                    , "plaintext"
                    ]
                ]
            , "implementation" .= Aeson.object []
            , "documentSymbol" .= Aeson.object []
            ]
        ]
