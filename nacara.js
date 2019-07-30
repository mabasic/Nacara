module.exports = {
    "githubURL": "https://github.com/MangelMaxime/Nacara",
    "url": "https://mangelmaxime.github.io",
    "baseUrl": "/Nacara/",
    "title": "Nacara",
    "debug": true,
    "changelog": "CHANGELOG.md",
    "version": "0.1.6",
    "navbar": {
        "showVersion": true,
        "community": "community",
        "links": [
            {
                "href": "/Nacara/index.html",
                "label": "Documentation",
                "icon": "fas fa-book"
            },
            {
                "href": "/Nacara/changelog.html",
                "label": "Changelog",
                "icon": "fas fa-tasks"
            },
            {
                "href": "https://gitter.im/fable-compiler/Fable",
                "label": "Support",
                "icon": "fab fa-gitter",
                "isExternal": true
            },
            {
                "href": "https://github.com/MangelMaxime/Nacara",
                "icon": "fab fa-github",
                "isExternal": true
            },
            {
                "href": "https://twitter.com/MangelMaxime",
                "icon": "fab fa-twitter",
                "isExternal": true,
                "color": "#55acee"
            }
        ]
    },
    "menu": {
        "Getting Started": [
            "index"
        ],
        "API": [
            "API/nacara-config-json",
            "API/page-attributes"
        ]
    },
    "lightner": {
        "backgroundColor": "#FAFAFA",
        "textColor": "",
        "themeFile": "./paket-files/akamud/vscode-theme-onelight/themes/OneLight.json",
        "grammars": [
            "./paket-files/ionide/ionide-fsgrammar/grammar/fsharp.json",
            "./paket-files/Microsoft/vscode/extensions/json/syntaxes/JSON.tmLanguage.json"
        ]
    }
};
