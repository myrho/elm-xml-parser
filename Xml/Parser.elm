module Xml.Parser exposing (parseXml, XmlAst(..))

{-| A parser which converts XML into an XmlAst,
which can be further transformed.

@docs XmlAst, parseXml
-}

import Combine exposing (..)
import Combine.Char exposing (..)
import String
import Result exposing (Result(..))


type alias Name =
    String


type alias Key =
    String


type alias Value =
    String


type alias Attribute =
    ( Key, Value )


{-| The XML AST representation
-}
type XmlAst
    = Element Name (List Attribute) (List XmlAst)
    | Body String
    | Comment String
    | CDATA String


spaces : Parser () (List Char)
spaces =
    many (space <|> newline <|> tab)


letter : Parser () Char
letter =
    upper <|> lower


betweenBoth : Char -> Parser () String
betweenBoth ch =
    String.fromList
        <$> between
                (char ch)
                (char ch)
                (many1 ((noneOf [ ch ])) <|> succeed [])


betweenSingleQuotes : Parser () String
betweenSingleQuotes =
    betweenBoth '\''


betweenDoubleQuotes : Parser () String
betweenDoubleQuotes =
    betweenBoth '"'


quotedString : Parser () String
quotedString =
    betweenSingleQuotes <|> betweenDoubleQuotes


attributeName : Parser () String
attributeName =
    String.fromList
        <$> many1 (letter <|> digit <|> char '-' <|> char ':')
        <?> "Invalid Attribute name"


tagName : Parser () String
tagName =
    String.fromList
        <$> many (choice [ letter, digit, char '_', char ':' ])
        <?> "Invalid Tag name"


keyValue : Parser () ( String, String )
keyValue =
    (\key value -> ( key, value ))
        <$> (attributeName <* spaces <* char '=' <* spaces)
        <*> (quotedString <* spaces)


openTag : Parser () ( String, List ( String, String ) )
openTag =
    (\name attribs -> ( name, attribs ))
        <$> (char '<' *> tagName)
        <*> (spaces *> many keyValue <* char '>')


closeTag : String -> Parser () ()
closeTag str =
    ()
        <$ (string "</" *> spaces *> string str *> spaces *> char '>')
        <?> ("Expected closing Tag for " ++ toString str)


withExplicitCloseTag : Parser () XmlAst
withExplicitCloseTag =
    (\( name, attribs, xml ) -> Element name attribs xml)
        <$> ((openTag <* spaces) |> andThen (\( name, attribs ) -> (\xml -> ( name, attribs, xml )) <$> (many (innerXml <* spaces) <* closeTag name)))


comment : Parser () XmlAst
comment =
    (String.fromList >> String.trim >> Comment)
        <$> (string "<!--" *> manyTill anyChar (string "-->"))


cdata : Parser () XmlAst
cdata =
    (String.fromList >> String.trim >> CDATA)
        <$> (string "<![CDATA[" *> manyTill anyChar (string "]]>"))


withoutExplicitCloseTag : Parser () XmlAst
withoutExplicitCloseTag =
    (\name attribs -> Element name attribs [])
        <$> ((char '<' *> tagName <* spaces))
        <*> (many keyValue <* string "/>")


parseBody : Parser () XmlAst
parseBody =
    (Body << String.trim << String.fromList) <$> (many1 (noneOf [ '<', '>' ]))


xmlDeclaration : Parser () ()
xmlDeclaration =
    () <$ (string "<?xml" *> Combine.while ((/=) '?') <* string "?>")


xmlParser : Parser () XmlAst
xmlParser =
    lazy (\() -> withExplicitCloseTag) <|> lazy (\() -> withoutExplicitCloseTag)


rootElements : Parser () (List XmlAst)
rootElements =
    many1 (choice [ xmlParser, comment <* spaces ])


innerXml : Parser () XmlAst
innerXml =
    comment <|> cdata <|> xmlParser <|> parseBody


parser : Parser () (List XmlAst)
parser =
    spaces *> maybe xmlDeclaration *> spaces *> rootElements <* spaces <* end


{-| Trys to parse the input string as an AST
-}
parseXml : String -> Result (List String) (List XmlAst)
parseXml str =
    case parse parser str of
        Ok ( _, _, xml ) ->
            Ok xml

        Err ( _, _, failure ) ->
            Err failure
