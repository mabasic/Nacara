module Nacara.Core.Types

open Thoth.Json
open Fable.Core
open Fable.React
open Node

#nowarn "21"
#nowarn "40"

type FrontMatterAttributes =
    class end

[<NoComparison>]
type PageContext =
    {
        PageId : string
        RelativePath : string
        FullPath : string
        Content : string
        Layout : string
        Section : string
        Title : string option
        Attributes : FrontMatterAttributes
    }

type LabelLink =
    {
        Section : string option
        Url : string
        IsPinned : bool
        Label : string
    }

module LabelLink =

    let decoder : Decoder<LabelLink> =
            Decode.object (fun get ->
                {
                    Section = get.Optional.Field "section" Decode.string
                    Url = get.Required.Field "url" Decode.string
                    Label = get.Required.Field "label" Decode.string
                    IsPinned = get.Optional.Field "pinned" Decode.bool
                            |> Option.defaultValue false
                }
            )


type IconLink =
    {
        Url : string
        Label : string
        Icon : string
    }

module IconLink =

    let decoder : Decoder<IconLink> =
            Decode.object (fun get ->
                {
                    Url = get.Required.Field "url" Decode.string
                    Label = get.Required.Field "label" Decode.string
                    Icon = get.Required.Field "icon" Decode.string
                }
            )

type DropdownLink =
    {
        Url : string
        Label : string
        Description : string option
    }

module DropdownLink =

    let decoder : Decoder<DropdownLink> =
        Decode.object (fun get ->
            {
                Url = get.Required.Field "url" Decode.string
                Label = get.Required.Field "label" Decode.string
                Description = get.Optional.Field "description" Decode.string
            }
        )

[<RequireQualifiedAccess>]
type DropdownItem =
    | Divider
    | Link of DropdownLink

module DropdownItem =

    let decoder : Decoder<DropdownItem> =
        Decode.oneOf [
            Decode.string
            |> Decode.andThen (function
                | "divider" ->
                    Decode.succeed DropdownItem.Divider

                | invalid ->
                    Decode.fail $"`{invalid}` is not a valid DropdownItem value. Did you mean 'spacer'?"
            )

            DropdownLink.decoder
            |> Decode.map DropdownItem.Link
        ]

type DropdownInfo =
    {
        Section : string option
        Label : string
        Items : DropdownItem list
        IsPinned : bool
        IsFullWidth : bool
    }

module DropdownInfo =

    let decoder : Decoder<DropdownInfo> =
        Decode.object (fun get ->
            {
                Section = get.Optional.Field "section" Decode.string
                Label = get.Required.Field "label" Decode.string
                Items = get.Required.Field "items" (Decode.list DropdownItem.decoder)
                IsPinned = get.Optional.Field "pinned" Decode.bool
                            |> Option.defaultValue false
                IsFullWidth = get.Optional.Field "fullwidth" Decode.bool
                            |> Option.defaultValue false
            }
        )

[<RequireQualifiedAccess>]
type StartNavbarItem =
    | LabelLink of LabelLink
    | Dropdown of DropdownInfo

module StartNavbarItem =

    let decoder : Decoder<StartNavbarItem> =
        Decode.oneOf [
            LabelLink.decoder
            |> Decode.map StartNavbarItem.LabelLink


            DropdownInfo.decoder
            |> Decode.map StartNavbarItem.Dropdown
        ]

type NavbarConfig =
    {
        Start : StartNavbarItem list
        End : IconLink list
    }

module NavbarConfig =

    let decoder : Decoder<NavbarConfig> =
        Decode.object (fun get ->
            {
                Start = get.Optional.Field "start" (Decode.list StartNavbarItem.decoder)
                            |> Option.defaultValue []
                End = get.Optional.Field "end" (Decode.list IconLink.decoder)
                        |> Option.defaultValue []
            }
        )

    let empty : NavbarConfig =
        {
            Start = []
            End = []
        }

type LightnerConfig =
    {
        BackgroundColor : string option
        TextColor : string option
        ThemeFile : string
        GrammarFiles : string list
    }

module LightnerConfig =

    let decoder : Decoder<LightnerConfig> =
        Decode.object (fun get ->
            {
                BackgroundColor = get.Optional.Field "backgroundColor" Decode.string
                TextColor = get.Optional.Field "textColor" Decode.string
                ThemeFile = get.Required.Field "themeFile" Decode.string
                GrammarFiles = get.Required.Field "grammars" (Decode.list Decode.string)
            }
        )

let private genericMsg msg value newLine =
    try
        "Expecting "
            + msg
            + " but instead got:"
            + (if newLine then "\n" else " ")
            + (Decode.Helpers.anyToString value)
    with
        | _ ->
            "Expecting "
            + msg
            + " but decoder failed. Couldn't report given value due to circular structure."
            + (if newLine then "\n" else " ")

let private errorToString (path : string, error) =
    let reason =
        match error with
        | BadPrimitive (msg, value) ->
            genericMsg msg value false
        | BadType (msg, value) ->
            genericMsg msg value true
        | BadPrimitiveExtra (msg, value, reason) ->
            genericMsg msg value false + "\nReason: " + reason
        | BadField (msg, value) ->
            genericMsg msg value true
        | BadPath (msg, value, fieldName) ->
            genericMsg msg value true + ("\nNode `" + fieldName + "` is unknown.")
        | TooSmallArray (msg, value) ->
            "Expecting " + msg + ".\n" + (Decode.Helpers.anyToString value)
        | BadOneOf messages ->
            "The following errors were found:\n\n" + String.concat "\n\n" messages
        | FailMessage msg ->
            "The following `failure` occurred with the decoder: " + msg

    match error with
    | BadOneOf _ ->
        // Don't need to show the path here because each error case will show it's own path
        reason
    | _ ->
        "Error at: `" + path + "`\n" + reason

let private unwrapWith (errors: ResizeArray<DecoderError>) path (decoder: Decoder<'T>) value: 'T =
    match decoder path value with
    | Ok v -> v
    | Error er -> errors.Add(er); Unchecked.defaultof<'T>

type MenuItemPage =
    {
        Label : string option
        PageId : string
    }

type MenuItemLink =
    {
        Label : string
        Href : string
    }

type MenuItemList =
    {
        Label : string
        Items : MenuItem list
        Collapsible : bool
        Collapsed : bool
    }

and [<RequireQualifiedAccess>] MenuItem =
    | Page of MenuItemPage
    | List of MenuItemList
    | Link of MenuItemLink

type Menu = MenuItem list

[<RequireQualifiedAccess>]
type FlatMenu =
    | Link of MenuItemLink
    | Page of MenuItemPage

module MenuItem =

    let rec decoder : Decoder<MenuItem> =
        Decode.oneOf [
            Decode.string
            |> Decode.map (fun pageId ->
                {
                    Label = None
                    PageId = pageId
                }
                |> MenuItem.Page
            )

            Decode.field "type" Decode.string
            |> Decode.andThen (function
                | "page" ->
                    Decode.object (fun get ->
                        {
                            Label = get.Optional.Field "label" Decode.string
                            PageId = get.Required.Field "pageId" Decode.string
                        }
                    )
                    |> Decode.map MenuItem.Page

                | "section" ->
                    Decode.object (fun get ->
                        {
                            Label = get.Required.Field "label" Decode.string
                            Items = get.Required.Field "items" (Decode.list decoder)
                            Collapsible = get.Optional.Field "collapsible" Decode.bool
                                            |> Option.defaultValue true
                            Collapsed = get.Optional.Field "collapsed" Decode.bool
                                            |> Option.defaultValue false
                        }
                    )
                    |> Decode.map MenuItem.List

                | "link" ->
                    Decode.object (fun get ->
                        {
                            Label = get.Required.Field "label" Decode.string
                            Href = get.Required.Field "href" Decode.string
                        }
                    )
                    |> Decode.map MenuItem.Link

                | invalidType ->
                    Decode.fail $"`%s{invalidType}` is not a valid type for a menu Item. Supported types are:\n- page\n- section\n- link"
            )
        ]

module Menu =

    let decoder : Decoder<Menu> =
        Decode.list MenuItem.decoder

type MenuConfig =
    {
        Section : string
        Items : Menu
    }

type Config =
    {
        FavIcon : string option
        WorkingDirectory : string
        Url : string
        SourceFolder : string
        BaseUrl : string
        Title : string
        EditUrl : string option
        Output : string
        Navbar : NavbarConfig
        LightnerConfig : LightnerConfig option
        Layouts : string array
        ServerPort : int
        IsWatch : bool
    }

    member this.DestinationFolder
        with get () =
            path.join(this.WorkingDirectory, this.Output)

// Minimal binding
type MarkdownIt =
    abstract render : string -> string


[<NoComparison; NoEquality>]
type RendererContext =
    {
        Config : Config
        SectionMenu : Menu option
        Menus : MenuConfig array
        Pages : PageContext array
        MarkdownToHtml : string -> JS.Promise<string>
        MarkdownToHtmlWithPlugins : (MarkdownIt -> MarkdownIt) -> string -> JS.Promise<string>
    }

type LayoutDependency =
    {
        Source : string
        Destination : string
    }

type LayoutRenderFunc = RendererContext -> PageContext -> JS.Promise<ReactElement>

[<NoComparison; NoEquality>]
type LayoutRenderer =
    {
        Name : string
        Func :  LayoutRenderFunc
    }

[<NoComparison; NoEquality>]
type LayoutInfo =
    {
        Dependencies : LayoutDependency array
        Renderers : LayoutRenderer array
    }

type LayoutInterface =
    abstract ``default`` : LayoutInfo with get

module Config =

    let decoder (cwd : string) (isWatch : bool) : Decoder<Config> =
        Decode.object (fun get ->
            {
                FavIcon = get.Optional.Field "favIcon" Decode.string
                WorkingDirectory = cwd
                Url = get.Required.Field "url" Decode.string
                BaseUrl = get.Required.Field "baseUrl" Decode.string
                Title = get.Required.Field "title" Decode.string
                SourceFolder = get.Optional.Field "source" Decode.string
                                |> Option.defaultValue "docs"
                EditUrl = get.Optional.Field "editUrl" Decode.string
                Output = get.Optional.Field "output" Decode.string
                            |> Option.defaultValue "docs_deploy"
                Navbar = get.Optional.Field "navbar" NavbarConfig.decoder
                            |> Option.defaultValue NavbarConfig.empty
                LightnerConfig = get.Optional.Field "lightner" LightnerConfig.decoder
                Layouts = get.Required.Field "layouts" (Decode.array Decode.string)
                ServerPort = get.Optional.Field "serverPort" Decode.int
                                |> Option.defaultValue 8080
                IsWatch = isWatch
            }
        )

[<RequireQualifiedAccess; NoComparison>]
type QueueFile =
    | Markdown of PageContext
    | Sass of filePath : string
    | JavaScript of filePath : string
    | LayoutDependency of LayoutDependency
    | Other of filePath : string
