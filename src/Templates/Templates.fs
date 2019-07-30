module Templates

open Fable.Core.JsInterop
open Types
open Fable.Import

type ITemplates =
    abstract DocPage : Model * PageContext -> React.ReactElement
    abstract Changelog : Model * Changelog.Types.Changelog -> React.ReactElement

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
