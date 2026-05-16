# --- 0) Packages
library(dataset) # your package
library(utils) # person()

# --- 1) Your three Europeana items (as provided)
items <- data.frame(
  europeana_id = c(
    "401/item_I42WBR2HJCSOEHIG66B72JSWP3IBK36X",
    "401/item_OMXACK7NJ6U6B3BZGPZCKMFHBKTJVDZY",
    "401/item_GU75RCP7ATGKK2DUIJ3RGNGB4QQLCJGI"
  ),
  title_et = c(
    "Foto. Meremäe Dobrova külakooli juures tegutsev laulukoor, mida juhatajas õpetaja Edgar Raudsepp.Setud esimest korda rahvariietes Tallinna laulupeol 1933(?)",
    "Setud (setu pruutpaar)",
    "Setud Meeksi kabeli juures."
  ),
  title_en = c(
    "Photo. Song chorus at Dobrova Village School, Mount Sea, chaired by teacher Edgar Ironsepp.Set for the first time in folk costumes at Tallinn Song Festival 1933 (?",
    NA_character_,
    NA_character_
  ),
  description_et = c(
    "sündmuse kommentaar: Setud esimest korda rahvariietes Tallinna laulupeol, esiplaanil M.Tammemägi, Eher ja Anne Lumi.",
    "sündmuse kommentaar: tiraaž 1000",
    NA_character_
  ),
  creator = c(
    "Unknown",
    "Eesti Rahva Muuseum (kirjastaja); Thomson, Woldemar (fotograaf); Trau ja Schwab Dresden Graphische Kunstanstalt (trükkija (valmistaja))",
    "ERKA (kopeerija); Parikas, Johannes-Georg (autor)"
  ),
  date_literal = c("1930", "1924", "umbes.1905"),
  year = c("1930", "1924", NA_character_),
  format_width = c("tervik laius: 13.5 cm", "tervik kujutise laius: 9.0 cm", "tervik laius: 17.5 cm"),
  format_height = c("tervik kõrgus: 8.4 cm", "tervik kujutise kõrgus: 14.0 cm", "tervik pikkus: 12.5 cm"),
  subject_et = c("foto", "postkaart", "foto"),
  subject_en = c("photograph", "postcard", "photograph"),
  spatial_labels = c("Võru; Eesti; Meremäe", "Saksamaa", "Eesti"),
  landingPage = c(
    "https://www.europeana.eu/item/401/item_I42WBR2HJCSOEHIG66B72JSWP3IBK36X",
    "https://www.europeana.eu/item/401/item_OMXACK7NJ6U6B3BZGPZCKMFHBKTJVDZY",
    "https://www.europeana.eu/item/401/item_GU75RCP7ATGKK2DUIJ3RGNGB4QQLCJGI"
  ),
  isShownAt = c(
    "https://www.muis.ee/museaalView/1095358",
    "https://www.muis.ee/museaalView/2160693",
    "https://www.muis.ee/museaalView/2705888"
  ),
  isShownBy = c(
    "https://www.muis.ee/digitaalhoidla/api/meedia/originaal?id=3b5c9066-51e3-4222-b3ea-6403c16b96c3",
    "https://www.muis.ee/digitaalhoidla/api/meedia/originaal?id=e1593727-cd06-46cf-8235-bf5d44376b72",
    "https://www.muis.ee/digitaalhoidla/api/meedia/originaal?id=e4c14f34-69e0-47b8-a9bf-218ac698144d"
  ),
  object = c(
    "https://www.muis.ee/digitaalhoidla/api/meedia/originaal?id=3b5c9066-51e3-4222-b3ea-6403c16b96c3",
    "https://www.muis.ee/digitaalhoidla/api/meedia/originaal?id=e1593727-cd06-46cf-8235-bf5d44376b72",
    "https://www.muis.ee/digitaalhoidla/api/meedia/originaal?id=e4c14f34-69e0-47b8-a9bf-218ac698144d"
  ),
  rights = c(
    "http://creativecommons.org/licenses/by/4.0/",
    "http://creativecommons.org/publicdomain/zero/1.0/",
    "http://creativecommons.org/licenses/by/4.0/"
  ),
  dataProvider_uri = c(
    "http://data.europeana.eu/organization/1684",
    "http://data.europeana.eu/organization/4473",
    "http://data.europeana.eu/organization/1955"
  ),
  dataProvider = c(
    "Võrumaa Muuseum",
    "Eesti Rahva Muuseum",
    "Tallinna Linnamuuseum"
  ),
  provider_uri = c(
    "http://data.europeana.eu/organization/1275",
    "http://data.europeana.eu/organization/1275",
    "http://data.europeana.eu/organization/1275"
  ),
  provider = c("E-Varamu", "E-Varamu", "E-Varamu"),
  edm_type = c("IMAGE", "IMAGE", "IMAGE"),
  stringsAsFactors = FALSE
)

# --- 2) Wrap with defined() + build dataset_df()
ds <- dataset_df(
  # Core CHO-ish labels (use DC)
  title_et = defined(items$title_et, label = "Pealkiri (ET)", concept = "http://purl.org/dc/terms/title"),
  title_en = defined(items$title_en, label = "Title (EN)", concept = "http://purl.org/dc/terms/title"),
  description_et = defined(items$description_et, label = "Kirjeldus (ET)", concept = "http://purl.org/dc/terms/description"),

  # Creators (free-text; keep semicolon-joined per row)
  creator = defined(items$creator, label = "Creator(s)", concept = "http://purl.org/dc/terms/creator"),

  # Dates
  date_literal = defined(items$date_literal, label = "Date literal", concept = "http://purl.org/dc/terms/date"),
  year = defined(items$year, label = "Year", concept = "http://www.europeana.eu/schemas/edm/year"),

  # Formats
  format_width = defined(items$format_width, label = "Format width", concept = "http://purl.org/dc/terms/format"),
  format_height = defined(items$format_height, label = "Format height", concept = "http://purl.org/dc/terms/format"),

  # Subjects & spatial
  subject_et = defined(items$subject_et, label = "Teema (ET)", concept = "http://purl.org/dc/terms/subject"),
  subject_en = defined(items$subject_en, label = "Subject (EN)", concept = "http://purl.org/dc/terms/subject"),
  spatial_labels = defined(items$spatial_labels, label = "Spatial (labels)", concept = "http://purl.org/dc/terms/spatial"),

  # Links (EDM)
  landingPage = defined(items$landingPage, label = "Europeana landing page", concept = "http://www.europeana.eu/schemas/edm/landingPage"),
  isShownAt = defined(items$isShownAt, label = "isShownAt", concept = "http://www.europeana.eu/schemas/edm/isShownAt"),
  isShownBy = defined(items$isShownBy, label = "isShownBy", concept = "http://www.europeana.eu/schemas/edm/isShownBy"),
  object = defined(items$object, label = "object", concept = "http://www.europeana.eu/schemas/edm/object"),
  rights = defined(items$rights, label = "rights", concept = "http://www.europeana.eu/schemas/edm/rights"),

  # Provider attribution (URIs + labels)
  dataProvider_uri = defined(items$dataProvider_uri, label = "dataProvider (URI)", concept = "http://www.europeana.eu/schemas/edm/dataProvider"),
  dataProvider = defined(items$dataProvider, label = "dataProvider (label)", concept = "http://www.europeana.eu/schemas/edm/dataProvider"),
  provider_uri = defined(items$provider_uri, label = "provider (URI)", concept = "http://www.europeana.eu/schemas/edm/provider"),
  provider = defined(items$provider, label = "provider (label)", concept = "http://www.europeana.eu/schemas/edm/provider"),

  # Type & identifier
  edm_type = defined(items$edm_type, label = "EDM Type", concept = "http://www.europeana.eu/schemas/edm/type"),
  europeana_id = defined(items$europeana_id, label = "Europeana ID", concept = "http://purl.org/dc/terms/identifier"),

  # Row URI prefix — tweak to your namespace/DOI if you like
  identifier = c(item = "https://example.org/dataset/seto-photos#"),

  # Minimal dataset-level bibentry
  dataset_bibentry = dublincore(
    title       = "Seto people: three example photos (Europeana-derived)",
    description = "A tiny EDM-aligned dataset built with dataset::defined() and dataset::dataset_df().",
    creator     = person("Your", "Name", role = "cre"),
    publisher   = "Your Org",
    language    = "en"
  )
)

# Peek
print(ds)
summary(ds)


nt_file <- tempfile(fileext = ".nt")
dataset_to_triples(ds, format = "nt")
g <- describe(ds, con = ttl_file) # if your package supports RDF/XML, switch to .rdf
message("Wrote Turtle to: ", ttl_file)
ttl_file
