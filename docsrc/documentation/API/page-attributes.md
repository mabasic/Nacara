---
title: Page attributes
layout: nacara-standard
---

[[toc]]

All your pages needs to provide a block called "Front Matter" at the top of their content:

```
---
title: Title of your page
---

Content of your page
```

## Mandatory fields

### `title` - [string]

This will be used as the title of your page. It can be used in the as a label in the menu or in the `title` tag of your page.

## Optional fields

### `id` - [string]

Use to override the `id` generated by default by Nacara.

By default, the `id` of a page is it's path inside the `source` folder without the extension.

If you have the follwing content in your `source` folder:

```
docsrc
├── API
│   ├── nacara-config-json.md
│   └── page-attributes.md
├── index.md
├── style.scss
```

Then:

- `index` refers to `index.md`
- `API/nacara-config-json` refers to `API/nacara-config-json.md`