---
title: Directory structure
layout: nacara-standard
---

A basic Nacara site looks to something like that:

```
├── docs # Source directory
│   ├── documentation
│   │   └── [...]
│   ├── scss
│   │   └── [...]
│   └── style.scss
├── docs_deploy
│   └── [...]
├── lightner
│   ├── grammars
│   └── themes
├── nacara.config.json
├── package.json
```

Overview, of what each of those does:

<table class="table is-narrow is-bordered is-vcentered">
    <thead>
        <tr>
            <th class="label-cell">File / Directory</th>
            <th class="label-cell">Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td class="label-cell">
                <code>docs</code>
            </td>
            <td class="fullwidth-cell">
                Default, folder where you place your website source files likes:
                <ul>
                    <li>Static resources</li>
                    <li>Markdown files to convert</li>
                    <li>Style files</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td class="label-cell">
                <code>docs/style.scss</code>
                <div class="is-size-7 my-2">or</div>
                <code>docs/style.sass</code>
            </td>
            <td class="fullwidth-cell">
                <p>
                    Main entry file for styling your application, generated file will be <code>docs_deploy/style.css</code>
                </p>
                <p>
                    Only one of these files can be used at a time.
                </p>
            </td>
        </tr>
        <tr>
            <td class="label-cell">
                <code>docs/scss</code>
                <div class="is-size-7 my-2">or</div>
                <code>docs/sass</code>
            </td>
            <td class="fullwidth-cell">
                This is where you place your SASS/SCSS files imported by <code>style.scss</code>.
            </td>
        </tr>
        <tr>
            <td class="label-cell">
                <code>docs_deploy</code>
            </td>
            <td class="fullwidth-cell">
                <p>This is where Nacara will put the generated files (by default).</p>
                <p>You should add this folder to your <code>.gitignore</code>.</p>
            </td>
        </tr>
        <tr>
            <td class="label-cell">
                <code>lightner/themes</code>
            </td>
            <td class="fullwidth-cell">
                <p>

Recommended place to place your theme file user by [Code-lightner](https://github.com/MangelMaxime/Code-Lightner) to provides code highlights.
                </p>
            </td>
        </tr>
        <tr>
            <td class="label-cell">
                <code>lightner/grammars</code>
            </td>
            <td class="fullwidth-cell">
                <p>

Recommended place to place your grammars files user by [Code-lightner](https://github.com/MangelMaxime/Code-Lightner) to provides code highlights.
                </p>
            </td>
        </tr>
        <tr>
            <td class="label-cell">
                <code>nacara.config.json</code>
            </td>
            <td class="fullwidth-cell">
                <p>Nacara config file.</p>
            </td>
        </tr>
        <tr>
            <td class="label-cell">
                <code>package.json</code>
            </td>
            <td class="fullwidth-cell">
                <p>File used to configure your NPM dependencies like Nacara, Babel, nacara-layout-standard.</p>
            </td>
        </tr>
        <tr>
            <td class="label-cell">
                <code>docs/index.md</code>
                <div class="is-size-7 my-2">or</div>
                <code>docs/\*\*/\*.md</code>
            </td>
            <td class="fullwidth-cell">
                <p>Any markdown file encounter are going to be transform by Nacara into HTML using the layout provided via the front-matter property.</p>
            </td>
        </tr>
        <tr>
            <td class="label-cell">
                <code>docs/\*\*/\*.md</code>
            </td>
            <td class="fullwidth-cell">
                <p>Any markdown file encounter are going to be transform by Nacara into HTML using the layout provided via the front-matter property.</p>
            </td>
        </tr>
        <tr>
            <td class="label-cell">
                <code>docs/\*\*/menu.json</code>
            </td>
            <td class="fullwidth-cell">
                Menu configuration for the section it is in.
            </td>
        </tr>
        <tr>
            <td class="label-cell">
                <code>Other files</code>
                <div class="is-size-7 my-2">or</div>
                <code>folders</code>
            </td>
            <td class="fullwidth-cell">
                Except for the special cases listed above, all the other file and folder will be copied into the destination folder.
            </td>
        </tr>
    </tbody>
</table>
