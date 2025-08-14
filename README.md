# Panblog

Lightweight static site generator (SSG) written in Haskell, powered by Pandoc.

## Blog source structure

```
.
├── templates/ # HTML templates & static assets
│ ├── page.html
│ ├── home.html
│ └── xxx.css
├── content/ # Blog posts (each folder = one post)
│ ├── post1/
│ │ ├── index.md
│ │ └── image.png
│ └── post2/
├── output/ # Generated HTML site (auto-created)
└── config.yaml # Global template variables (title, author, etc.)
```