[<AutoOpen>]
module Global

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

[<Emit("require($0)")>]
let require<'T> (modulePath : string) : 'T = jsNative

let isNotNull (o : 'T) =
    not (isNull o)

module Directory =

    let moveUp (path : string) =
        path.Split(char Node.Exports.path.sep)
        |> Array.skip 1
        |> String.concat Node.Exports.path.sep

    let join (pathA : string) (pathB : string) =
        Node.Exports.path.join(pathA, pathB)

    let exists (dir : string) =
        Promise.create (fun resolve reject ->
            Node.Exports.fs.exists((U2.Case1 dir), (fun res ->
                resolve res
            ))
        )

    let create (dir : string) =
        let options = createObj [
            "recursive" ==> true
        ]

        Promise.create (fun resolve reject ->
            Node.Exports.fs?mkdir(dir, options, (fun (err : Node.Base.NodeJS.ErrnoException option) ->
                match err with
                | Some err -> reject (err :?> System.Exception)
                | None -> resolve ()
            ))
        )

    let ensure (dir: string) =
        promise {
            match! exists dir with
            | true -> return ()
            | false ->
                return! create dir
        }

    let dirname (dir : string) =
        Node.Exports.path.dirname(dir)


    let getFiles (dir : string) =
        Promise.create (fun resolve reject ->
            Node.Exports.fs.readdir(U2.Case1 dir, (fun (err: Node.Base.NodeJS.ErrnoException option) (files : ResizeArray<string>) ->
                match err with
                | Some err ->
                    reject (err :?> System.Exception)
                | None ->
                    files.ToArray()
                    |> resolve
            ))
        )

module File =
    let changeExtension (extention : string) (path : string) =
        let extensionPos = path.LastIndexOf('.')
        path.Substring(0, extensionPos + 1) + extention

    let read (path: string) =
        Promise.create (fun resolve reject ->
            Node.Exports.fs.readFile(path, (fun err buffer ->
                match err with
                | Some err -> reject (err :?> System.Exception)
                | None -> resolve (buffer.toString())
            ))
        )

    let readSync (path: string) =
        Node.Exports.fs.readFileSync(path).toString()

    let write (path: string) (content: string) =
        promise {
            do! path |> Directory.dirname |> Directory.ensure
            return!
                Promise.create (fun resolve reject ->
                    Node.Exports.fs.writeFile(path, content, (fun res ->
                        match res with
                        | Some res -> reject (res :?> System.Exception)
                        | None -> resolve ()
                    ))
                )
        }

    let exist (path : string) =
        Promise.create (fun resolve reject ->
            Node.Exports.fs.exists(U2.Case1 path, (fun res ->
                resolve res
            ))
        )

    let existSync (path : string) =
        Node.Exports.fs.existsSync(U2.Case1 path)

    let absolutePath (dir : string) =
        Node.Exports.path.resolve(dir)



module Helpers =

    open System.Collections.Generic
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.Import
    open Fable.Import.Node
    open Fable.Import.Node.Globals

    let markdown (_:string) : string = importMember "./js/utils.js"

    /// Resolves a path to prevent using location of target JS file
    /// Note the function is inline so `__dirname` will belong to the calling file
    let inline resolve (path: string) =
        Exports.path.resolve(__dirname, path)

    /// Parses a React element invoking ReactDOMServer.renderToString
    let parseReact (el: React.ReactElement) =
        ReactDomServer.renderToString el

    /// Parses a React element invoking ReactDOMServer.renderToStaticMarkup
    let parseReactStatic (el: React.ReactElement) =
        ReactDomServer.renderToStaticMarkup el

    let unEscapeHTML (unsafe : string) =
        unsafe
            .Replace("&amp;", "&")
            .Replace("&lt;", "<")
            .Replace("&gt;", ">")
            .Replace("&quot;", "\"")
            .Replace("&#039;", "'")

    open Fable.Core
    open Fable.Helpers.React
    open Fable.Helpers.React.Props
    open Fulma

    // type DangerousInnerHtml =
    //     { __html : string }

    // let htmlFromMarkdown str =
    //     promise {
    //         let! html = makeHtml str
    //         return div [ DangerouslySetInnerHTML { __html = html } ] [ ]
    //     }

    // let contentFromMarkdown str =
    //     promise {
    //         let! html = makeHtml str
    //         return Content.content [ Content.Props [ DangerouslySetInnerHTML { __html = html } ] ]
    //             [ ]
    //     }

    let whitespace =
        span [ DangerouslySetInnerHTML { __html = " " } ]
            [ ]
