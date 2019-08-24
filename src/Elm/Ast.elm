module Elm.Ast exposing (Node(..), fromCode, toString)

import Parser exposing
    ( (|=), (|.)
    , Parser, Nestable(..), Step(..), Trailing(..)
    )
import Set exposing (Set)


type Node
    = BlankLine
    | Comment
    | CustomType String (List Variant)
    | ImportDeclaration String (Maybe String) (List ToplevelName)
    | ModuleDeclaration String (List ToplevelName)
    | TypeAlias String (List RecordField)


type ToplevelName
    = Function String

type Variant
    = SimpleVariant String

type RecordField
    = RecordField String TypeDeclaration

type TypeDeclaration
    = IntType

fromCode : String -> List Node
fromCode code =
    let
        declaration =
            Parser.oneOf
                [ moduleDeclaration
                , importDeclaration
                , typeAlias
                , customType
                , Parser.succeed Comment |. comment
                ]

        declarationParser (oldOffset, revDecls) =
            Parser.oneOf
                [ Parser.succeed (\decl offset -> if offset == oldOffset then Done (offset, List.reverse revDecls) else Loop (offset, decl :: revDecls))
                    |= declaration
                    |. Parser.spaces
                    |= Parser.getOffset
                , Parser.succeed ()
                    |> Parser.map (\_ -> Done (oldOffset, List.reverse revDecls))
                ]
    in
    case Parser.run (Parser.loop (0, []) declarationParser)  code of
        Ok (_, decls) ->
            decls

        Err problems ->
            Debug.log ("Parser problems: " ++ Debug.toString problems) []


moduleDeclaration : Parser Node
moduleDeclaration =
    Parser.succeed ModuleDeclaration
        |. Parser.spaces
        |. Parser.keyword "module"
        |. Parser.spaces
        |= moduleName
        |. Parser.spaces
        |. Parser.keyword "exposing"
        |. Parser.spaces
        |= moduleExports
        |. Parser.spaces


importDeclaration : Parser Node
importDeclaration =
    Parser.succeed (\modName aka imports -> ImportDeclaration modName aka imports)
        |. Parser.keyword "import"
        |. Parser.spaces
        |= moduleName
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.keyword "as"
                |. Parser.spaces
                |= moduleNameWithoutDots
                |. Parser.spaces
            , Parser.succeed Nothing
            ]
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.keyword "exposing"
                |. Parser.spaces
                |= moduleImports
                |. Parser.spaces
            , Parser.succeed []
            ]
        |. Parser.spaces

typeAlias : Parser Node
typeAlias =
    Parser.succeed TypeAlias
        -- FIXME: `type alias` should probably work with arbitrary whitespace
        |. Parser.keyword "type alias"
        --|. Parser.spaces
        --|. Parser.keyword "alias"
        |. Parser.spaces
        |= typeAliasName
        |. Parser.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= Parser.sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , spaces = Parser.spaces
            , item = recordField
            , trailing = Forbidden
            }
        |. Parser.spaces

typeAliasName : Parser String
typeAliasName =
    Parser.variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c
        , reserved = reservedWords
        }

recordField : Parser RecordField
recordField =
    Parser.succeed RecordField
        |= recordFieldName
        |. Parser.spaces
        |. Parser.symbol ":"
        |. Parser.spaces
        |= typeDeclaration
        |. Parser.spaces

recordFieldName : Parser String
recordFieldName =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = reservedWords
        }

typeDeclaration : Parser TypeDeclaration
typeDeclaration =
    Parser.succeed IntType
        |. Parser.keyword "Int"
        |. Parser.spaces

customType : Parser Node
customType =
    Parser.succeed CustomType
        |. Parser.keyword "type"
        |. Parser.spaces
        |= customTypeName
        |. Parser.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , separator = "|"
            , end = ""
            , spaces = Parser.spaces
            , item = variant
            , trailing = Forbidden
            }

customTypeConstructor : Parser String
customTypeConstructor =
    Parser.variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c
        , reserved = reservedWords
        }

customTypeName : Parser String
customTypeName =
    Parser.variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c
        , reserved = reservedWords
        }

variant : Parser Variant
variant =
    Parser.succeed SimpleVariant
        |= customTypeConstructor
        |. Parser.spaces

--blankLines : Parser Node
--blankLines =
--    (Parser.loop 0 <| ifProgress <|
--        (Parser.oneOf
--            [ Parser.chompWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')
--            , Parser.spaces
--            ]
--        )
--    )
--    |> Parser.map (\_ -> BlankLine)


comment : Parser ()
comment =
    Parser.loop 0 <| ifProgress <|
        Parser.oneOf
            [ Parser.lineComment "--"
            , Parser.multiComment "{-" "-}" Nestable
            , Parser.spaces
            ]


ifProgress : Parser a -> Int -> Parser (Step Int ())
ifProgress parser offset =
    Parser.succeed identity
        |. parser
        |= Parser.getOffset
        |> Parser.map
            (\newOffset -> if offset == newOffset then Done () else Loop newOffset)


moduleName : Parser String
moduleName =
    Parser.variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c || c == '.'
        , reserved = reservedWords
        }


moduleNameWithoutDots : Parser String
moduleNameWithoutDots =
    Parser.variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c
        , reserved = reservedWords
        }


moduleImports : Parser (List ToplevelName)
moduleImports =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = Parser.spaces
        , item = toplevelNameParser
        , trailing = Forbidden
        }


moduleExports : Parser (List ToplevelName)
moduleExports =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = Parser.spaces
        , item = toplevelNameParser
        , trailing = Forbidden
        }


toplevelNameParser : Parser ToplevelName
toplevelNameParser =
    Parser.succeed Function
        |= Parser.variable
            { start = \it -> Char.isUpper it || Char.isLower it
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = reservedWords
            }


reservedWords : Set String
reservedWords =
    Set.fromList
        [ "if"
        , "then"
        , "else"
        , "case"
        , "of"
        , "let"
        , "in"
        , "type"
        , "module"
        , "where"
        , "import"
        , "exposing"
        , "as"
        , "port"
        ]


toString : List Node -> String
toString nodes =
    let
        nodeToString node =
            case node of
                BlankLine ->
                    "(BlankLine)"

                Comment ->
                    "(Comment)"

                CustomType typeName variants ->
                    let
                        variantToString v =
                            case v of
                                SimpleVariant n ->
                                    " (SimpleVariant " ++ n ++ ")"
                        vs =
                            variants
                                |> List.map variantToString
                                |> String.join ""
                    in
                    "(CustomType " ++ typeName ++ vs ++ ")"

                ImportDeclaration modName maybeAlias imports ->
                    let
                        importToString toImport =
                            case toImport of
                                Function name ->
                                    "(Function " ++ name ++ ")"

                        imported =
                            List.map importToString imports
                                |> String.join " "
                                |> (\s -> if String.length s > 0 then " " ++ s else s)

                        aka =
                            maybeAlias
                                |> Maybe.map (\it -> " (Alias " ++ it ++ ")")
                                |> Maybe.withDefault " (NoAlias)"
                    in
                    "(ImportDeclaration " ++ modName ++ aka ++ imported ++ ")"

                ModuleDeclaration modName exports ->
                    let
                        exportToString export =
                            case export of
                                Function name ->
                                    "(Function " ++ name ++ ")"

                        exported =
                            List.map exportToString exports
                                |> String.join " "
                                |> (\s -> if String.length s > 0 then " " ++ s else s)
                    in
                    "(ModuleDeclaration " ++ modName ++ exported ++ ")"

                TypeAlias typeName fields ->
                    let
                        fieldToString (RecordField k v) =
                            case v of
                                IntType ->
                                    " (RecordField " ++ k ++ " Int)"

                        record =
                            List.map fieldToString fields
                                |> String.join ""
                                
                    in
                    "(TypeAlias " ++ typeName ++ record ++ ")"
    in
    List.map nodeToString nodes
        |> String.join "\n"


zeroOrMore : Parser a -> Parser (List a)
zeroOrMore item =
    Parser.loop [] (zeroOrMoreHelp item)


zeroOrMoreHelp : Parser a -> List a -> Parser (Step (List a) (List a))
zeroOrMoreHelp item revItems =
    Parser.oneOf
        [ Parser.succeed (\a -> Loop (a :: revItems))
            |= item
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.reverse revItems))
        ]

