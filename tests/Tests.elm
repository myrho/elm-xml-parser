module Tests exposing (..)

import Test exposing (Test, describe)
import Expect exposing (Expectation)
import String
import Xml.Parser exposing (parseXml, XmlAst(..))

{- Run all tests. The `test` function is defined at the end of this file. -}
all : Test
all =
    describe "Xml.Parser" [ prolog, names, elements ]


{-| Empty document
-}
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


{-| NAMES Names for tags and attributes have the same rules.

[4] NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] |
    [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] |
    [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] |
    [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
[4a] NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] |
    [#x203F-#x2040]
[5] Name ::= NameStartChar (NameChar)*

Just testing the basics for now; no Unicode
-}
names : Test
names =
    let
        nameStartChars =
            [ ":", "A", "_", "z" ]

        nameChars =
            nameStartChars ++ [ "", "-", ".", "0" ]

        validNames =
            List.concatMap
                (\nsc -> List.map (\nc -> nsc ++ nc) nameChars)
                nameStartChars

        invalidNames =
            List.drop (List.length nameStartChars) nameChars
                ++ [ ",", "@", "*", "#" ]
    in
        describe "Names"
            [ describe "Invalid names"
                (List.concatMap
                    (\n ->
                        [ test "Element"
                            [ "<" ++ n, "/>" ]
                            (Ok [ Element n [] [] ])
                        , test "Attribute"
                            [ "<a ", n, "=\"\"/>" ]
                            (Ok [ Element "a" [ ( n, "" ) ] [] ])
                        ]
                    )
                    validNames
                )
            , describe "Invalid names"
                (List.concatMap
                    (\n ->
                        [ test "Element"
                            [ "<" ++ n, "/>" ]
                            (Err [ "Invalid Tag name", "Invalid Tag name", "expected \"<!--\"" ])
                        , test "Attribute"
                            [ "<a ", n, "=\"\"/>" ]
                            (Err [ "expected '>'", "expected \"/>\"", "expected \"<!--\"" ])
                        ]
                    )
                    invalidNames
                )
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
        , describe "Attributes"
            [ test "Empty attribute"
                [ "<a ", "b", "=", "\"\"", "/>" ]
                (Ok
                    [ Element "a" [ ( "b", "" ) ] [] ]
                )
            , test "1 attribute"
                [ "<a ", "b", "=", "\"1\"", "/>" ]
                (Ok
                    [ Element "a" [ ( "b", "1" ) ] [] ]
                )
            , test "2 attributes"
                [ "<a ", "b", "=", "\"1\"", "c", "=", "\"2\"", "/>" ]
                (Ok
                    [ Element "a" [ ( "b", "1" ), ( "c", "2" ) ] [] ]
                )
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


{-| Takes a test name, a list of string parts, and an expected result, and
creates a test. The string parts will be joined together, interspersed with
various types of whitespace to create an input string, each of which will be
tested.
-}
test : String -> List String -> Result (List String) (List XmlAst) -> Test
test name parts expected =
    let
        inputList =
            -- S ::= (#x20 | #x9 | #xD | #xA)+
            [ "", " ", "  ", "\t", "\n" ]
                |> List.map (\s -> String.join s parts)
    in
        inputList
            |> List.map
                (\input ->
                    Test.test input
                        (\() -> parseXml input |> Expect.equal expected)
                )
            |> describe name
