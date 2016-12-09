module Tests exposing (..)

import Test exposing (Test, describe)
import Expect exposing (Expectation)
import String
import Xml.Parser exposing (parseXml, XmlAst(..))


all : Test
all =
    describe "Xml.Parser" [ prolog, elements ]

empty : Test
empty =
    describe "Empty document"
        [ test "Empty or whitespace"
            [ "", "" ]
            (Ok [])
        ]

{- PROLOG

[22] prolog ::= XMLDecl? Misc* (doctypedecl Misc*)?
[23] XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
[24] VersionInfo ::= S 'version' Eq (' VersionNum ' | " VersionNum ")
[25] Eq ::= S? '=' S?
[26] VersionNum ::= ([a-zA-Z0-9_.:] | '-')+
[27] Misc ::= Comment | PI | S
-}
prolog : Test
prolog =
    describe "Prolog"
        [ test "Xml Declaration not in put in AST"
            [ "<?xml ", "version", "=", "1.0", "?><a/>" ]
            (Ok [ Element "a" [] [] ])

        -- Todo: An xml declaration followed by something that isn't a well
        -- formed document should probably be an error. Currently:
        --   - xmldecl followed by nothing gives a parsing error
        --   - xmldecl followed by text gives a parsing error
        --   - xmldecl followed by a comment parses ok (bug?)
        --   - xmldecl followed by a cdata section gives a parsing error
        ]


{-| Elements
-}
elements : Test
elements =
    describe "Elements"
        [ describe "Empty"
            [ test "No end tag"
                [ "<a", "/>" ]
                (Ok
                    [ Element "a" [] [] ]
                )
            , test "With end tag"
                [ "<a", ">", "</a>" ]
                (Ok
                    [ Element "a" [] [] ]
                )
            ]
        , describe "Names" 
            [ -- todo
            ]
        , describe "Attributes"
            [ test "1 attribute"
                [ "<a ", "b", "=", "\"1\"", "/>" ]
                (Ok
                    [ Element "a" [ ( "b", "1" ) ] [] ]
                )
            , test "2 attributes"
                [ "<a ", "b", "=", "\"1\"", "c", "=", "\"2\"", "/>" ]
                (Ok
                    [ Element "a" [ ( "b", "1" ), ( "c", "2" ) ] [] ]
                )
            , describe "Names"
                [ -- todo
                ]
            ]
        , describe "Content"
            [ test "Text"
                [ "<a>", "b  c", "</a>" ]
                (Ok
                    [ Element "a" [] [ Body "b  c" ] ]
                )
            , test "Mixed"
                [ "<a>", "<b></b>", "<!-- c -->", "d", "<![CDATA[ e ]]>", "</a>" ]
                (Ok
                    [ Element "a"
                        []
                        [ Element "b" [] []
                        , Comment "c"
                        , Body "d"
                        , CDATA "e"
                        ]
                    ]
                )
            ]
        ]


test : String -> List String -> Result (List String) (List XmlAst) -> Test
test name parts expected =
    let
        inputList =
            -- S ::= (#x20 | #x9 | #xD | #xA)+
            [ "", " ", "  ", "\t", "\r", "\n" ]
                |> List.map (\s -> String.join s parts)
    in
        inputList
            |> List.map
                (\input ->
                    Test.test input
                        (\() -> parseXml input |> Expect.equal expected)
                )
            |> describe name
