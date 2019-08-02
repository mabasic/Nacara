module Templates

open Fable.Core.JsInterop
open Types
open Fable.React

type ITemplates =
    abstract DocPage : Model * PageContext -> ReactElement
    abstract Changelog : Model * Changelog.Types.Changelog -> ReactElement

type IExport =
    abstract Centered : ITemplates

exportDefault
    { new IExport with
        member __.Centered =
            { new ITemplates with
                member __.DocPage (model, pageContext) = Templates.Centered.DocPage.toHtml model pageContext
                member __.Changelog (model, pageContext) = Templates.Centered.Changelog.toHtml model pageContext
            }
    }
