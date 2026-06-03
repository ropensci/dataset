test_that("n_triple and related helpers produce correct N-Triple strings", {
  # String literal object
  triple_1 <- n_triple(
    "http://example.org/show/218",
    "http://www.w3.org/2000/01/rdf-schema#label",
    "That Seventies Show"
  )

  # Language-tagged literal object
  triple_2 <- n_triple(
    "http://example.org/show/218",
    "http://example.org/show/localName",
    '"Cette S<U+00E9>rie des Ann<U+00E9>es Septante"@fr-be'
  )

  # XSD datetime literal from POSIXct
  triple_time <- n_triple(
    "http://example.com/creation",
    "http://www.w3.org/ns/prov#generatedAtTime",
    as.POSIXct(10000, origin = "2024-01-01", tz = "UTC")
  )

  # ORCID agent as subject
  triple_agent <- n_triple(
    "https://orcid.org/0000-0001-7513-6760",
    "a",
    "http://www.w3.org/ns/prov#Agent"
  )

  # Person object with ORCID
  author_person <- person(
    given = "Daniel",
    family = "Antal",
    comment = c(ORCID = "https://orcid.org/0000-0001-7513-6760")
  )

  # Expectations
  expect_equal(
    triple_1,
    '<http://example.org/show/218> <http://www.w3.org/2000/01/rdf-schema#label> "That Seventies Show"^^<http://www.w3.org/2001/XMLSchema#string> .'
  )

  expect_equal(
    triple_time,
    '<http://example.com/creation> <http://www.w3.org/ns/prov#generatedAtTime> "2024-01-01T02:46:40Z"^^<xsd:dateTime> .'
  )

  expect_equal(length(n_triples(c(triple_1, triple_2, triple_1))), 2)
  expect_equal(length(n_triples(c(triple_1, triple_2))), 2)

  expect_equal(
    triple_agent,
    "<https://orcid.org/0000-0001-7513-6760> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> ."
  )

  # create_iri() datatype coercion checks
  expect_equal(create_iri(23), '"23"^^<http://www.w3.org/2001/XMLSchema#double>')
  expect_equal(create_iri("23"), '"23"^^<http://www.w3.org/2001/XMLSchema#string>')
  expect_equal(create_iri(as.integer(23)), '"23"^^<http://www.w3.org/2001/XMLSchema#integer>')
  expect_equal(create_iri(as.Date("2024-10-30")), '"2024-10-30"^^<http://www.w3.org/2001/XMLSchema#date>')

  # prov_author
  expect_equal(
    prov_author(author_person),
    "<https://orcid.org/0000-0001-7513-6760> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> ."
  )
})

test_that("n_triple() gives error", {
  expect_error(n_triple(s = c("1", "2"), p = "author", o = "Jane Doe"),
    regexp = "a scalar value"
  )
})


test_that("expand_triples handles multiple values and filtering", {
  dataset_id <- "http://example.org/demo"
  predicate <- "http://purl.org/dc/terms/creator"

  # Test with single string
  single <- expand_triples(dataset_id, predicate, "Jane Doe")
  expect_type(single, "character")
  expect_length(single, 1)
  expect_true(grepl("Jane Doe", single))

  # Test with multiple strings
  multi <- expand_triples(dataset_id, predicate, c("Jane Doe", "John Smith"))
  expect_length(multi, 2)
  expect_true(any(grepl("Jane Doe", multi)))
  expect_true(any(grepl("John Smith", multi)))

  # Test with person objects
  creators <- c(
    person(given = "Ada", family = "Lovelace"),
    person(given = "Grace", family = "Hopper")
  )
  people <- expand_triples(dataset_id, predicate, creators)
  expect_length(people, 2)
  expect_true(any(grepl("Lovelace", people)))
  expect_true(any(grepl("Hopper", people)))

  # Test filtering of placeholder values
  placeholders <- expand_triples(dataset_id, predicate, c(":tba", ":unas", "", NA, NULL))
  expect_length(placeholders, 0)

  # Mixed valid and invalid
  mixed <- expand_triples(dataset_id, predicate, c("Valid Name", ":tba", NA, "", ":unas"))
  expect_length(mixed, 1)
  expect_true(grepl("Valid Name", mixed))
})


test_that("create_iri() helper function", {
  author_person <- person(
    given = "Daniel", family = "Antal",
    email = "daniel.antal@dataobservatory.eu",
    role = c("aut", "cre"),
    comment = c(ORCID = "0000-0001-7513-6760")
  )
  expect_error(create_iri(list(a = 1:2)))
  # expect_output(print(create_iri(as.POSIXct(10000, origin = "2024-01-01", tz="UTC"))), "2024-01-01T03:46:40Z")
  # expect_output(print(create_iri(as.POSIXct(10000, origin = "2024-01-01", tz="UTC"))), "\\^\\^<xsd:dateTime>")
  expect_equal(create_iri(author_person), "<https://orcid.org/0000-0001-7513-6760>")
  jane_doe <- person(given = "Jane", family = "Doe", role = "aut", email = "example@example.com")
  joe_doe <- person(
    given = "Joe", family = "Doe", role = "aut", email = "example@example.com",
    comment = c(Wikidata = "https://www.wikidata.org/wiki/Q000")
  )
  expect_equal(create_iri(x = joe_doe), "<https://www.wikidata.org/wiki/Q000>")
  joe_doe <- person(
    given = "Joe", family = "Doe", role = "aut", email = "example@example.com",
    comment = c(ISNI = "1234")
  )
  expect_equal(create_iri(x = joe_doe), "<https://isni.org/isni/1234>")
  viaf_doe <- person(
    given = "Joe", family = "Doe", role = "aut", email = "example@example.com",
    comment = c(VIAF = "1234")
  )
  expect_equal(create_iri(x = viaf_doe), "<https://viaf.org/viaf/1234>")
  expect_equal(create_iri(x = 2), "\"2\"^^<http://www.w3.org/2001/XMLSchema#double>")
  expect_true(grepl("http://www.w3.org/2001/XMLSchema#date>", create_iri(Sys.Date())))
})

test_that("get_person_iri() provides the reformatted iri for the persons", {
  author_person <- person(
    given = "Daniel", family = "Antal",
    email = "daniel.antal@dataobservatory.eu",
    role = c("aut", "cre"),
    comment = c(ORCID = "0000-0001-7513-6760")
  )
  expect_equal(get_person_iri(author_person), "https://orcid.org/0000-0001-7513-6760")
  expect_equal(get_person_iri(person("Jane Doe")), NULL)
  expect_equal(
    get_person_iri(p = person(
      given = "Daniel", family = "Antal",
      role = "cre", comment = c(ORCID = "0000-0001-7513-6760")
    )),
    "https://orcid.org/0000-0001-7513-6760"
  )
  expect_equal(
    get_person_iri(
      p =
        person(
          given = "Edgar", family = "Anderson",
          role = "cre", comment = c(VIAF = "https://viaf.org/viaf/6440526")
        )
    ),
    c(VIAF = "https://viaf.org/viaf/6440526")
  )
  expect_equal(
    get_person_iri(
      p =
        person(given = "Taylor", family = "Swift", role = "cre", comment = c(ISNI = "https://isni.org/isni/0000000078519858"))
    ),
    c(ISNI = "https://isni.org/isni/0000000078519858")
  )
})

test_that("create_iri() handles scalar types correctly", {
  # Numeric (double)
  expect_equal(create_iri(3.14), '"3.14"^^<http://www.w3.org/2001/XMLSchema#double>')

  # Integer
  expect_equal(create_iri(as.integer(42)), '"42"^^<http://www.w3.org/2001/XMLSchema#integer>')

  # Date
  expect_equal(create_iri(as.Date("2024-01-01")), '"2024-01-01"^^<http://www.w3.org/2001/XMLSchema#date>')

  # POSIXct
  dt <- as.POSIXct("2024-01-01 12:34:56", tz = "UTC")
  expect_equal(create_iri(dt), '"2024-01-01T12:34:56Z"^^<xsd:dateTime>')

  # URI string
  expect_equal(create_iri("https://example.org/id"), "<https://example.org/id>")

  # Language-tagged literal
  expect_equal(create_iri('"Some value"@en'), '"\"Some value\"@en"')

  # Blank node (now unquoted)
  expect_equal(create_iri("_:anon1"), "_:anon1")

  # RDF type shortcut
  expect_equal(create_iri("a"), "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>")

  # DCMI type abbreviation
  expect_equal(
    create_iri("DCMITYPE:Dataset"),
    '"http://purl.org/dc/dcmitype/Dataset"^^<http://www.w3.org/2001/XMLSchema#string>'
  )

  # Plain string
  expect_equal(create_iri("plain string"), '"plain string"^^<http://www.w3.org/2001/XMLSchema#string>')
})

test_that("create_iri() handles person() objects correctly", {
  # Person with VIAF
  p1 <- person("Jane", "Doe", comment = c(viaf = "12345"))
  expect_equal(create_iri(p1), "<https://viaf.org/viaf/12345>")

  # Person with ORCID
  p2 <- person("J", "Doe", comment = c(orcid = "0000-0001-2345-6789"))
  expect_equal(create_iri(p2), "<https://orcid.org/0000-0001-2345-6789>")
})

test_that("create_iri() fallback person gets a string literal", {
  p3 <- person("Harper", "Lee", role = "aut", email = "test@example.com")
  iri_p3 <- create_iri(p3)

  expect_match(iri_p3, '^"[^"]+"\\^\\^<http://www.w3.org/2001/XMLSchema#string>$')
  expect_true(grepl("Harper|Lee", iri_p3))
})

test_that("create_iri() fails for vector inputs", {
  expect_error(create_iri(c("a", "b")), "input must be a scalar value")
})

test_that("create_iri() fails for list-like or tabular inputs", {
  expect_error(create_iri(list(a = 1)), "must be a scalar")
  expect_error(create_iri(data.frame(x = 1)), "must be a scalar")
})


test_that("prov_author() handles different person identifiers correctly", {
  # ORCID
  p_orcid <- person("Jane", "Doe", comment = c(ORCID = "0000-0002-1825-0097"))
  triple_orcid <- prov_author(p_orcid)
  expect_match(triple_orcid, "<https://orcid.org/0000-0002-1825-0097> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> \\.")

  # ISNI
  p_isni <- person("Alan", "Smith", comment = c(ISNI = "0000000121032683"))
  triple_isni <- prov_author(p_isni)
  expect_match(triple_isni, "<https://isni.org/isni/0000000121032683> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> \\.")

  # VIAF
  p_viaf <- person("Nick", "Carraway", comment = c(VIAF = "12345678"))
  triple_viaf <- prov_author(p_viaf)
  expect_match(triple_viaf, "<https://viaf.org/viaf/12345678> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> \\.")

  # Wikidata
  p_qid <- person("Fitzgerald", "Scott", comment = c(wikidata = "Q39232"))
  triple_qid <- prov_author(p_qid)
  expect_match(
    triple_qid,
    "^<https://www\\.wikidata\\.org/wiki/Q39232> <http://www\\.w3\\.org/1999/02/22-rdf-syntax-ns#type> <http://www\\.w3\\.org/ns/prov#Agent> \\.$"
  )

  # Fallback to blank node with label
  p_fallback <- person("Harper", "Lee", role = "aut")
  triple_fallback <- prov_author(p_fallback)
  expect_match(
    triple_fallback,
    "^_:[a-z]+ <http://www\\.w3\\.org/1999/02/22-rdf-syntax-ns#type> <http://www\\.w3\\.org/ns/prov#Agent> \\.$"
  )

  # Multiple persons
  multi <- personList(p_orcid, p_isni)
  triples_multi <- prov_author(multi)
  expect_length(triples_multi, 2)
  expect_true(all(grepl("^<https://", triples_multi)))
})
