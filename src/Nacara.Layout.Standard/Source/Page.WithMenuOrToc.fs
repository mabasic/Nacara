module Page.WithMenuOrToc

open Nacara.Core.Types
open Feliz
open Feliz.Bulma

let private renderTableOfContents (tableOfContent : TableOfContentParser.Header list) =
    if tableOfContent.Length > 0 then
        Html.li [
            Html.ul [
                prop.className "table-of-content"

                prop.children [
                    for tocElement in tableOfContent do
                        Html.li [
                            Html.a [
                                prop.dangerouslySetInnerHTML tocElement.Title
                                prop.href tocElement.Link
                                prop.custom("data-toc-element", true)
                            ]
                        ]
                ]
            ]
        ]
    else
        null

let private renderMenuItemPage
    (config : Config)
    (pages : PageContext array)
    (info : MenuItemPage)
    (currentPageId : string)
    (tocInformation : TableOfContentParser.Header list) =

    let labelText =
        match info.Label with
        | Some label ->
            label

        | None ->
            let pageContext =
                pages
                |> Array.tryFind (fun pageContext ->
                    pageContext.PageId = info.PageId
                )
                |> function
                    | Some pageContext ->
                        pageContext

                    | None ->
                        failwith $"Page of id '%s{info.PageId}' not found. You either need to create it or remove it from the menu.json file"

            match pageContext.Title with
            | Some title ->
                title

            | None ->
                failwith $"Page of id '%s{info.PageId}' doesn't have a label set for the menu. You can provide one by using 'label' in the menu.json or adding a 'title' property to the front matter of the file"

    let isCurrentPage =
        info.PageId = currentPageId

    React.fragment [
        Bulma.menuItem.a [
            prop.classes [
                if isCurrentPage then
                    "is-active"
            ]

            prop.href (config.SiteMetadata.BaseUrl + info.PageId + ".html")
            prop.text labelText
        ]

        if isCurrentPage then
            renderTableOfContents tocInformation
    ]

/// <summary>
/// Render sub-menu
/// </summary>
let rec private renderSubMenu
    (config : Config)
    (pages : PageContext array)
    (menu : Menu)
    (currentPageId : string)
    (tocInformation : TableOfContentParser.Header list) =

    menu
    |> List.map (
        function
        | MenuItem.Link info ->
            Bulma.menuItem.a [
                prop.className "menu-external-link"
                prop.href info.Href
                prop.text info.Label
                prop.target.blank
            ]

        | MenuItem.Page info ->
            renderMenuItemPage config pages info currentPageId tocInformation

        | _ -> Html.none
    )

/// <summary>
/// Render menu from the top level
/// </summary>
let rec private renderMenu
    (config : Config)
    (pages : PageContext array)
    (menu : Menu)
    (currentPageId : string)
    (tocInformation : TableOfContentParser.Header list) =

    let menuContent =
        menu
        |> List.map (
            function
            | MenuItem.Link info ->
                Bulma.menuList [
                    Bulma.menuItem.a [
                        prop.href info.Href
                        prop.text info.Label
                        prop.target.blank
                    ]
                ]

            | MenuItem.Page info ->
                Bulma.menuList [
                    Html.li [
                        renderMenuItemPage config pages info currentPageId tocInformation
                    ]
                ]

            | MenuItem.List info ->
                React.fragment [
                    Bulma.menuLabel info.Label

                    Bulma.menuList [
                        yield! renderSubMenu config pages info.Items currentPageId tocInformation
                    ]
                ]
        )

    Html.div [
        prop.className "menu-container"

        prop.children [
            Bulma.menu [
                prop.children menuContent
            ]
        ]
    ]


let private renderPageWithMenuOrTableOfContent
    (breadcrumbElement : ReactElement)
    (menuElement : ReactElement)
    (pageContent : ReactElement) =

    Bulma.container [
        breadcrumbElement

        Bulma.columns [
            columns.isGapless
            columns.isMobile

            prop.children [
                Bulma.column [
                    prop.className "is-menu-column"
                    column.is3Desktop
                    helpers.isHiddenTouch

                    prop.children [
                        menuElement
                    ]
                ]

                Bulma.column [
                    column.is8Desktop
                    column.isFullTouch

                    prop.children [
                        pageContent
                    ]
                ]
            ]
        ]
    ]


let private renderPageWithoutMenuOrTableOfContent (pageContent : ReactElement) =

    Bulma.container [
        Bulma.columns [
            columns.isMobile

            prop.children [
                Bulma.column [
                    column.is8Desktop
                    column.isOffset2Desktop

                    prop.children [
                        pageContent
                    ]
                ]
            ]
        ]
    ]

let private renderTableOfContentOnly
    (tocInformation : TableOfContentParser.Header list) =

    Html.div [
        prop.className "menu-container"

        prop.children [
            Bulma.menu [
                Bulma.menuList [

                    Bulma.menuItem.a [
                        prop.className "is-active"
                        prop.text "Table of content"
                    ]

                    renderTableOfContents tocInformation
                ]
            ]
        ]
    ]

let rec tryFindTitlePathToCurrentPage
    (pageContext : PageContext)
    (acc : string list)
    (menu : Menu) =

    match menu with
    | head :: tail ->
        match head with
        // Skip this item as it doesn't represent a page
        | MenuItem.Link _ ->
            tryFindTitlePathToCurrentPage pageContext acc tail

        | MenuItem.List info ->
            match tryFindTitlePathToCurrentPage pageContext (acc @ [ info.Label ]) info.Items with
            | Some res ->
                Some res

            | None ->
                tryFindTitlePathToCurrentPage pageContext acc tail

        | MenuItem.Page info ->
            if info.PageId = pageContext.PageId then
                let menuLabel =
                    Helpers.getMenuLabel pageContext info

                Some (acc @ [ menuLabel ])
            else
                tryFindTitlePathToCurrentPage pageContext acc tail

    | [ ] ->
        None

let renderBreadcrumbItems (items : string list) =
    items
    |> List.map (fun item ->
        Html.li [
            // Make the item active to make it not clickable
            prop.className "is-active"

            prop.children [
                Html.a [
                    prop.text item
                ]
            ]
        ]
    )

let private renderBreadcrumb
    (navbar : NavbarConfig)
    (pageContext : PageContext)
    (menu : Menu) =

    match tryFindTitlePathToCurrentPage pageContext [ ] menu with
    | None ->
        null

    | Some titlePath ->
        let titlePath =
            match Navbar.tryFindWebsiteSectionLabelForPage navbar pageContext with
            | Some sectionLabel ->
                sectionLabel @ titlePath

            | None ->
                titlePath

        Html.div [
            prop.className "mobile-menu"

            prop.children [
                Bulma.breadcrumb [
                    Html.ul [
                        Html.li [
                            Html.a [
                                prop.className "menu-trigger"

                                prop.children [
                                    Html.span [  ]
                                    Html.span [  ]
                                    Html.span [  ]
                                ]
                            ]
                        ]

                        yield! renderBreadcrumbItems titlePath
                    ]
                ]
            ]
        ]


[<NoComparison>]
type RenderArgs =
    {
        Config : Config
        SectionMenu : Menu option
        Pages : PageContext array
        PageContext : PageContext
        PageHtml : string
        PageContent : ReactElement
        RenderMenu : bool
    }

let render (args : RenderArgs) =

    let tocInformation =
        TableOfContentParser.parse args.PageHtml

    if args.RenderMenu then
        match args.SectionMenu, tocInformation.IsEmpty with
        // If there is a menu, we render it with the menu
        // The menu renderer will take care of generating the TOC elements if needed
        | Some sectionMenu, false
        | Some sectionMenu, true ->
            renderPageWithMenuOrTableOfContent
                (renderBreadcrumb args.Config.Navbar args.PageContext sectionMenu)
                (renderMenu args.Config args.Pages sectionMenu args.PageContext.PageId tocInformation)
                args.PageContent

        | None, false ->
            renderPageWithMenuOrTableOfContent
                null // No breadcrumb because there is no menu
                (renderTableOfContentOnly tocInformation)
                args.PageContent

        | None, true ->
            renderPageWithMenuOrTableOfContent
                null
                null
                args.PageContent

    // Layout forced to not render the menu, so only try to render the page with a TOC
    else
        if tocInformation.IsEmpty then
            renderPageWithMenuOrTableOfContent
                null
                null
                args.PageContent

        else
            renderPageWithMenuOrTableOfContent
                null // No breadcrumb because there is no menu
                (renderTableOfContentOnly tocInformation)
                args.PageContent
