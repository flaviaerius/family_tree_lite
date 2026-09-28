# Family Tree Lite

An interactive family tree viewer built with [Shiny](https://shiny.posit.co/). Upload a CSV with one row per person (and, optionally, a folder of photos) and the app draws the whole family as a clickable tree: people are colored by family branch, couples are joined, and children hang from their parents.

> The app interface is in Brazilian Portuguese ("Árvore Genealógica").

## Live app

The app is publicly available at **https://flaviaerius.shinyapps.io/family_tree_lite/**.

Nothing is stored on the server: the CSV and photos you upload live only in your browser session and are discarded when you close the app (see [Privacy](#privacy)).

## Features

- **Full-family overview** — every person as a circle, laid out by generation, colored by branch (`nucleus`), with partners and parent–child links drawn.
- **Search** — find a person by name; the sidebar shows their card (full name, birthday, photo, parents, partner).
- **Focused tree** — click a person's card to open a zoomed view with just that person, their parents, partner and children.
- **Click to explore** — clicking a circle in the overview fills the sidebar card, the same as searching.
- **Photos (optional)** — upload a folder of JPG/PNG files; each photo is cropped to a circle automatically. People without a photo show the initial of their name.
- **Missing parents handled** — a parent or partner named in the CSV who has no row of their own gets a placeholder so no branch is left dangling in the drawing. Placeholders are not searchable or clickable.
- **Built-in example** — the **Usar CSV exemplo** button loads a sample family so you can try the app without preparing any data.

## Using the app

Click **Como usar** in the top bar at any time to reopen the in-app instructions.

### 1. The CSV

Comma-separated, UTF-8 encoded (so accents work), with these column names in the first row:

| Column         | Required | Description                                                                                          |
| -------------- | :------: | ---------------------------------------------------------------------------------------------------- |
| `name`         |   yes    | Full name. It is the key: it must be unique, and mother/father/partner are linked through it.        |
| `sex`          |   yes    | `M` or `F`.                                                                                          |
| `generation`   |   yes    | Integer: `1` for the oldest generation, `2` for their children, and so on.                           |
| `nucleus`      |   yes    | Name of the family branch. Determines the person's color.                                            |
| `birth_year`   |   yes    | Year of birth. May be left empty if `birth_date` is filled in.                                       |
| `short_name`   |    no    | How the name appears inside the circle. Empty: the first name is used.                               |
| `birth_date`   |    no    | Full date as `YYYY-MM-DD` or `DD/MM/YYYY`. Shown as a birthday on the card.                          |
| `death_year`   |    no    | Year of death.                                                                                       |
| `mother_name`  |    no    | Mother's full name, spelled exactly as in her own `name` cell.                                       |
| `father_name`  |    no    | Father's full name, same rule.                                                                       |
| `partner_name` |    no    | Partner's full name, same rule.                                                                      |
| `image_file`   |    no    | Photo file name including extension (e.g. `maria.jpg`).                                              |

Empty cells can be blank or `NA`.

If siblings share a mother who has no row of her own, write `placeholder_1` in `mother_name` for all of them (and `placeholder_2` for half-siblings with a different unknown mother, and so on).

Example:

```csv
name,sex,generation,nucleus,birth_year,short_name,birth_date,death_year,mother_name,father_name,partner_name,image_file
Maria Cavalcante,F,1,Fundadores,1940,Maria,12/03/1940,,,,Fulano da Silva,maria.jpg
Fulano da Silva,M,1,Fundadores,1938,Fulano,,2010,,,Maria Cavalcante,fulano.jpg
Ana da Silva,F,2,Ramo Ana,1965,Ana,1965-07-09,,Maria Cavalcante,Fulano da Silva,,ana.png
```

A larger sample lives in [tests/fixtures/family_sample.csv](tests/fixtures/family_sample.csv).

### 2. Photos (optional)

- Formats: JPG or PNG.
- Pick the **whole folder** at once, not file by file.
- File names must match the `image_file` column (case-insensitive).
- Square images with the face centered work best.
- Total upload size is capped at 60 MB (`shiny.maxRequestSize` in [global.R](global.R)).

## Privacy

Uploaded photos are written to a per-session temporary folder, cropped, embedded in the page as base64 images and deleted when the session ends. The CSV is read from the upload and kept in memory for the session only. The app does not persist or publish anything you upload.

## Running locally

### Requirements

- [R](https://cran.r-project.org/) 4.5
- [rv](https://github.com/A2-ai/rv), the R package manager used to pin dependencies ([rproject.toml](rproject.toml) and [rv.lock](rv.lock))

Install `rv`:

```sh
curl -sSL https://raw.githubusercontent.com/A2-ai/rv/refs/heads/main/scripts/install.sh | bash
```

### Setup

```sh
git clone git@github.com:flaviaerius/family_tree_lite.git
cd family_tree_lite

# Install the exact package versions from rv.lock into ./rv/library
rv sync
```

The project's [.Rprofile](.Rprofile) activates the `rv` library automatically, so start R from the project root.

### Run

```sh
R -e "shiny::runApp()"
```

Or, from an R session started in the project root, `shiny::runApp()`. The app opens at `http://127.0.0.1:<port>`. Click **Usar CSV exemplo** to see it working right away.

### Tests

The tests must be run from the `tests/` directory, and need a UTF-8 locale because the assertions contain accented characters:

```sh
cd tests
LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'testthat::test_dir(".")'
```

## Running with Docker

The [Dockerfile](Dockerfile) builds on `rocker/r-ver:4.5`, installs `rv`, restores the locked packages and serves the app on port **8080** (the port Cloud Run expects).

```sh
docker build -t family-tree-lite .
docker run --rm -p 8080:8080 family-tree-lite
```

Then open <http://localhost:8080>.

## Deployment

The same image can be deployed to any container platform, including [Google Cloud Run](https://cloud.google.com/run). The repository also contains a `rsconnect/` folder from a previous [shinyapps.io](https://www.shinyapps.io/) deployment (it is git-ignored).

## Project layout

```
app.R                  UI and server
global.R               Libraries and global options (upload size limit)
R/
  load_family_csv.R    CSV parsing and validation
  placeholders.R       Adds rows for parents cited but missing from the CSV
  layout_overview.R    Tree layout for the whole family
  layout_focused.R     Tree layout for a single person
  plot_overview.R      Plotly figure for the overview
  plot_focused.R       Plotly figure for the focused tree
  person_card.R        Sidebar card for the selected person
  uploaded_images.R    Matches uploaded photos to people, per session
  crop_circle.R        Crops photos into circles
  images.R             Base64 image helpers
  help_modal.R         "Como usar" instructions modal
  colors.R, utils_*.R  Palettes and helpers
www/tree_scale.js      Scales labels with the browser zoom
tests/                 testthat suite and sample CSV
rproject.toml          rv project definition (R version, repositories, packages)
rv.lock                Locked package versions
```

## License

[MIT](LICENSE) © Flávia Eichemberger Rius
