#' Read FAOTradeMatrix
#'
#' Read in FAOSTAT detail trade matrix.
#' FAOSTAT does not balance or harmonize the import/export side reporting.
#' Furthermore, in terms of trade value, exporters are "usuallY" reporting FOB, while importers report CIF.
#' Difference in value, given identical qty,
#' is thus the transport margin and any unharmonized reporting combined.
#' @param subtype subsets of the detailed trade matrix to read in. Very large csv needs to be read in chunks
#' separated by export/import quantities and values, as well as kcr, kli and kothers (not in kcr nor kli)
#' Options are all combinations of c("import_value", "import_qty", "export_value",
#' "export_qty" X c("kcr", "kli", "kothers", "kforestry"))
#' import is import side reporting while export is export-sde reporting
#' @return FAO data as MAgPIE object
#' @author David C
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("FAOTradeMatrix", "import_value_kcr")
#' }
#' @importFrom tidyr pivot_longer starts_with unite
#' @importFrom dplyr summarise filter group_by ungroup %>% distinct inner_join
#' @importFrom magpiesets findset

readFAOTradeMatrix <- function(subtype) { # nolint

  forestry <- length(grep("kforestry", subtype)) == 1

  if (forestry) {
    file <- "Forestry_Trade_Flows_E_All_Data_(Normalized).csv"
  } else {
    file <- "Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv"
  }

  # ---- Define the subtype selection ----

  # This is resolved up front (rather than at the very end) so that the rows which do not belong to the
  # requested trade element can be dropped directly after reading. Only about a quarter of the rows of the
  # detailed trade matrix belong to any one element, so filtering early keeps the expensive per-row work
  # (ISO matching, item renaming, joining) off the ~75% of rows that would be discarded anyway.
  if (!forestry) {
    kcr <- findset("kcr")
    kli <- findset("kli")
    kothers <- setdiff(findset("kall"), c(kcr, kli))

    elements <- list(
      import_value_kcr = list(trade = "import_kUS$", product = kcr),
      import_value_kli = list(trade = "import_kUS$", product = kli),
      import_value_kothers = list(trade = "import_kUS$", product = kothers),
      import_qty_kcr = list(trade = c("import", "Import_Quantity_(1000_Head)",
                                      "Import_Quantity_(Head)", "Import_Quantity_(no)"),
                            product = kcr),
      import_qty_kli = list(trade = c("import", "Import_Quantity_(1000_Head)",
                                      "Import_Quantity_(Head)", "Import_Quantity_(no)"),
                            product = kli),
      import_qty_kothers = list(trade = c("import", "Import_Quantity_(1000_Head)",
                                          "Import_Quantity_(Head)", "Import_Quantity_(no)"),
                                product = kothers),
      export_value_kcr = list(trade = "export_kUS$", product = kcr),
      export_value_kli = list(trade = "export_kUS$", product = kli),
      export_value_kothers = list(trade = "export_kUS$", product = kothers),
      export_qty_kcr = list(trade = c("export", "Export_Quantity_(1000_Head)",
                                      "Export_Quantity_(Head)", "Export_Quantity_(no)"),
                            product = kcr),
      export_qty_kli = list(trade = c("export", "Export_Quantity_(1000_Head)",
                                      "Export_Quantity_(Head)", "Export_Quantity_(no)"),
                            product = kli),
      export_qty_kothers = list(trade = c("export", "Export_Quantity_(1000_Head)",
                                          "Export_Quantity_(Head)", "Export_Quantity_(no)"),
                                product = kothers)
    )
  } else {
    elements <- list(
      import_value_kforestry = list(trade = "Import_Value_(1000_USD)"),
      import_qty_kforestry = list(trade = c("import", "import_m3")),
      export_value_kforestry = list(trade = "Export_Value_(1000_USD)"),
      export_qty_kforestry = list(trade = c("export", "export_m3"))
    )
  }

  element <- toolSubtypeSelect(subtype, elements)

  # ---- Select columns to be read from file and read file ----

  ## efficient reading of csv file: read only needed columns in the needed type (codes as factor)
  csvcolnames <- colnames(read.table(file, header = TRUE, nrows = 1, sep = ","))

  # check if data is in long or wide format
  long <- ifelse("Year" %in% csvcolnames, TRUE, FALSE)

  # define vector with types corresponding to the columns in the file
  readcolClass <- rep("NULL", length(csvcolnames))
  # Element and Unit are read as factors: they only ever hold a handful of distinct values and reading them
  # as factors lets the ElementShort construction below work on the levels instead of on every row
  factorCols <- c("Reporter.Country.Code", "Partner.Country.Code", "Item.Code", "Element.Code",
                  "Element", "Unit")
  readcolClass[csvcolnames %in% factorCols] <- "factor"
  readcolClass[csvcolnames %in% c("Area", "Country", "Item",
                                  "Months", "Reporter.Countries", "Partner.Countries")] <- "character"
  readcolClass[csvcolnames %in% c("Value", "Year")] <- NA
  if (!long) {
    readcolClass[grepl("Y[0-9]{4}$", csvcolnames)] <- NA
  }

  fao <- data.table::fread(input = file, header = FALSE, skip = 1, sep = ",",
                           colClasses = readcolClass,
                           col.names = csvcolnames[is.na(readcolClass) | readcolClass != "NULL"],
                           quote = "\"",
                           encoding = "Latin-1", showProgress = FALSE)
  fao <- as.data.frame(fao)
  # from wide to long (move years from individual columns into one column)
  if (!long) {
    fao <- pivot_longer(fao, cols = starts_with("Y"), names_to = "Year", names_pattern = "Y(.*)",
                        names_transform = list("Year" = as.integer), values_to = "Value")
  }

  names(fao)[names(fao) == "Reporter.Country.Code"] <- "ReporterCountryCode"
  names(fao)[names(fao) == "Partner.Country.Code"] <- "PartnerCountryCode"
  names(fao) <- gsub("\\.", "", names(fao))

  # ---- Reformat elements and drop everything outside the requested trade element ----

  elementShort <- toolGetMapping("FAOelementShort.csv", where = "mrfaocore")
  # keep relevant rows only
  elementShort <- elementShort[elementShort$ElementCode %in% levels(fao$ElementCode), ]

  unitLv <- levels(fao$Unit)
  elemLv <- levels(fao$Element)
  codeLv <- levels(fao$ElementCode)

  # FAO renamed units between data releases ("tonnes" -> "t", "1000 US$" -> "1000 USD").
  # Only rename when the file at hand actually uses the new spelling, so older downloads keep working.
  unitRenames <- c("tonnes" = "t", "1000 US$" = "1000 USD")
  for (oldUnit in names(unitRenames)) {
    newUnit <- unitRenames[[oldUnit]]
    if (oldUnit %in% elementShort$Unit && !(oldUnit %in% unitLv) && newUnit %in% unitLv) {
      elementShort$Unit[elementShort$Unit == oldUnit] <- newUnit
    }
  }

  # ElementShort is a pure function of (ElementCode, Element, Unit), which together take only a few dozen
  # distinct values. Build the lookup once over the grid of factor levels and address it per row with an
  # integer index, so no full-length character vector is ever constructed.
  grid <- expand.grid(unit = unitLv, element = elemLv, code = codeLv,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  # make ElementShort a combination of Element and Unit, replace special characters, and replace multiple _ by one
  tmpElement <- gsub("[\\.,;?\\+& \\/\\-]", "_", grid$element, perl = TRUE)
  tmpUnit    <- gsub("[\\.,;\\+& \\-]", "_",    grid$unit, perl = TRUE)
  gridShort <- gsub("_{1,}", "_", paste0(tmpElement, "_(", tmpUnit, ")"), perl = TRUE) # nolint

  ### replace ElementShort with the entries from ElementShort if the Unit is the same
  if (nrow(elementShort) > 0) {
    # rev() reproduces the "last matching mapping row wins" behaviour of the original per-row loop
    ord <- rev(seq_len(nrow(elementShort)))
    idx <- match(paste(grid$code, grid$unit, sep = "\r"),
                 paste(elementShort$ElementCode[ord], elementShort$Unit[ord], sep = "\r"))
    overridden <- !is.na(idx)
    gridShort[overridden] <- as.character(elementShort$ElementShort)[ord][idx[overridden]]
  }

  # position of each row within the (unit, element, code) grid, matching expand.grid's ordering
  gridIndex <- (as.integer(fao$ElementCode) - 1L) * length(elemLv) * length(unitLv) +
    (as.integer(fao$Element) - 1L) * length(unitLv) +
    as.integer(fao$Unit)

  keep <- (gridShort %in% element$trade)[gridIndex]
  keep[is.na(keep)] <- FALSE
  if (!any(keep)) {
    stop("No rows left after selecting element(s) ", paste(element$trade, collapse = ", "),
         " for subtype ", subtype, ". Elements available in ", file, ": ",
         paste(sort(unique(gridShort)), collapse = ", "))
  }
  fao <- fao[keep, , drop = FALSE]
  fao$ElementShort <- gridShort[gridIndex[keep]]

  # ---- Assigning the ISO codes to countries ----

  # Load FAO specific countries (not included in country2iso.csv in madrat)
  faoIsoFaoCodeMapping <- toolGetMapping("FAOiso_faocode_online.csv", where = "mrfaocore")
  # convert data frame into named vector as required by toolCountry2isocode
  faoIsoFaoCode <- as.character(faoIsoFaoCodeMapping$ISO)
  names(faoIsoFaoCode) <- as.character(faoIsoFaoCodeMapping$Country)

  fao$ReporterISO <- toolCountry2isocode(fao$ReporterCountries, mapping = faoIsoFaoCode)
  fao$PartnerISO <- toolCountry2isocode(fao$PartnerCountries, mapping = faoIsoFaoCode,
                                        ignoreCountries = c("Others (adjustment)", "Total FAO",
                                                            "Unspecified Area"))

  # Drop rows with missing ISO code (shouldn't be any) together with the small islands that collapse onto
  # the same ISO3 code, in a single subsetting step instead of three consecutive copies of the data frame
  droppedIslands <- c("Johnston Island", "Midway Island", "Canton and Enderbury Islands", "Wake Island")
  fao <- fao[!is.na(fao$ReporterISO) & !is.na(fao$PartnerISO) &
               !fao$ReporterCountries %in% droppedIslands &
               !fao$PartnerCountries %in% droppedIslands, , drop = FALSE]

  # ---- Reformat items ----

  # remove accent in Mate to avoid problems and remove other strange names
  # Item holds only a few hundred distinct values, so the renaming is done on the unique items and mapped back
  uItem <- unique(fao$Item)
  tItem <- gsub("é", "e", uItem, perl = TRUE)
  tItem <- gsub("\n + (Total)", " + (Total)", tItem, fixed = TRUE)
  itemPos <- match(fao$Item, uItem)
  fao$Item <- tItem[itemPos]

  fao <- unite(fao, col = "ISO", c("ReporterISO", "PartnerISO"), sep = ".", remove = FALSE)

  if (!forestry) {
    # guard the empty case: paste0() would recycle to length 1 if no row matched the requested element
    fao$ItemCodeItem <- if (nrow(fao) == 0L) {
      character(0)
    } else {
      paste0(fao$ItemCode, "|", gsub("\\.", "", tItem, perl = TRUE)[itemPos])
    }

    # subset by product column
    mapping <- toolGetMapping("FAO_trade_k_mapping.csv", type = "sectoral", where = "mrfaocore")
    mapping <- mapping[, c("post2010_FAOoriginalItem_fromWebsite", "k")]
    colnames(mapping)[1] <- "ItemCodeItem"
    mapping <- distinct(mapping)

    out <- inner_join(fao, mapping, by = "ItemCodeItem", relationship = "many-to-many")
    out <- out[out$k %in% element$product, , drop = FALSE]

  } else {
    out <- unite(fao, col = "ItemCodeItem", c("ItemCode", "Item"), sep = "|", remove = FALSE)
  }

  out <- as.magpie(out[, c("Year", "ISO", "ItemCodeItem", "ElementShort", "Value")],
                   temporal = 1, spatial = 2, datacol = 5)   # import/export unit is in tonnes
  getItems(out, dim = 1, raw = TRUE) <- gsub("_", ".", getItems(out, dim = 1))

  out <- magpiesort(out)

  return(out)
}
