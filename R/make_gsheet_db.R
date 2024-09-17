library(gsheet)
library(dplyr)
library(readxl)

#Read in Carnegie data sheets ----
carnegie_val <-
  readxl::read_excel("./data-raw/CCIHE2021-PublicData.xlsx", sheet = "Values")
colnames(carnegie_val) <- c("Variable", "Label", "Value", "Value_Label")
carnegie_val <- carnegie_val[!is.na(carnegie_val$Value_Label), ]

carnegie_dat <-
  readxl::read_excel("./data-raw/CCIHE2021-PublicData.xlsx", sheet = "Data")
carnegie_dat <- dplyr::select(carnegie_dat,
                              !c(basic2000, basic2005, basic2010,
                                 basic2015, basic2018))

carnegie_var <-
  readxl::read_excel("./data-raw/CCIHE2021-PublicData.xlsx", sheet = "Variable")

#Fill in empty rows ----
fill_vals <- function(x) {
  if(is.na(x[1])) {stop("first value must not be NA")}
  for (i in 1:length(x)) {if(is.na(x[i])) {x[i] <- x[i-1]}}
  return(x)
}
carnegie_val$Variable <- fill_vals(carnegie_val$Variable)
carnegie_val$Label <- fill_vals(carnegie_val$Label)

carnegie_val <- dplyr::filter(carnegie_val,
                              !Variable %in% c("Basic2000", "BASIC2005",
                                               "BASIC2010", "BASIC2015",
                                               "BASIC2018"))
#Label R1 and R2 ----
carnegie_val <- dplyr::mutate(
  carnegie_val,
  Value_Label = ifelse(Variable == "BASIC2021",
                       ifelse(Value == 15,
                              paste("R1", Value_Label),
                              ifelse(Value == 16,
                                     paste("R2", Value_Label),
                                     Value_Label)),
                       Value_Label))

carnegie_dat <- as.data.frame(carnegie_dat)
carnegie_val <- as.data.frame(carnegie_val)
carnegie_var <- as.data.frame(carnegie_var)

#Convert numeric codes into meaningful labels ----
for(i in 1:ncol(carnegie_dat)) {
  col <- colnames(carnegie_dat)[i]
  if(any(grepl(pattern = paste0("^", col, "$"), carnegie_val$Variable,
               ignore.case = TRUE))) {
    temp <- carnegie_val[grep(pattern = paste0("^", col, "$"), carnegie_val$Variable,
                              ignore.case = TRUE), ]
    carnegie_dat[, i] <- temp$Value_Label[match(carnegie_dat[, i], temp$Value)]
  }
}

#Update column names to be long-form understandable labels ----
colnames(carnegie_dat) <-
  sapply(colnames(carnegie_dat),
         mytable = carnegie_var$Variable,
         mylabels = carnegie_var$Label,
         function(clnm, mytable, mylabels) {
           mylabels[grep(pattern = paste0("^", clnm, "$"),
                         x = mytable, ignore.case = TRUE)]})

carnegie_dat <- select(carnegie_dat,
                       "Institution name",
                       "State abbreviation",
                       "2021 Basic Classification")

colnames(carnegie_dat)[1] <- "Carnegie Institution Name"
carnegie_dat <- cbind(data.frame("Alias" = carnegie_dat$`Carnegie Institution Name`),
                      carnegie_dat)

#Add aliases for variants ----
#Manually chosen rules include:
#   Treat "The" from beginning of Carnegie as optional
carnegie_dat <-
  rbind(carnegie_dat,
        mutate(filter(carnegie_dat, grepl("^The ", Alias)),
               Alias = gsub("^The ", "", Alias)))
#   Treat " at " anywhere in Carnegie as optional
carnegie_dat <-
  rbind(carnegie_dat,
        mutate(filter(carnegie_dat, grepl(" at ", Alias)),
               Alias = gsub(" at ", " ", Alias)))
#   Carnegie uses "-" with no spaces, but ecoevo sometimes
#     uses ", " or " " or " - "
carnegie_dat <-
  rbind(carnegie_dat,
        mutate(filter(carnegie_dat, grepl("-", Alias)),
               Alias = gsub("-", ", ", Alias)),
        mutate(filter(carnegie_dat, grepl("-", Alias)),
               Alias = gsub("-", " ", Alias)),
        mutate(filter(carnegie_dat, grepl("-", Alias)),
               Alias = gsub("-", " - ", Alias)))
#Carnegie uses all of Saint, St., and St.
# Replace each variant with the other two possibilities
carnegie_dat <-
  rbind(carnegie_dat,
        mutate(filter(carnegie_dat, grepl(" Saint ", Alias)),
               Alias = gsub(" Saint ", " St. ", Alias)),
        mutate(filter(carnegie_dat, grepl(" Saint ", Alias)),
               Alias = gsub(" Saint ", " St ", Alias)),
        mutate(filter(carnegie_dat, grepl(" St. ", Alias)),
               Alias = gsub(" St. ", " Saint ", Alias)),
        mutate(filter(carnegie_dat, grepl(" St. ", Alias)),
               Alias = gsub(" St. ", " St ", Alias)),
        mutate(filter(carnegie_dat, grepl(" St ", Alias)),
               Alias = gsub(" St ", " Saint ", Alias)),
        mutate(filter(carnegie_dat, grepl(" St ", Alias)),
               Alias = gsub(" St ", " St. ", Alias))
        )

#Concatenate state at end of name
carnegie_dat <- mutate(
  carnegie_dat,
  Alias = paste(Alias, state.name[match(`State abbreviation`, state.abb)]))

#Add manually curated aliases ----
aliases <- read.csv("./data-raw/aliases.csv", fileEncoding = "UTF-8",
                    strip.white = TRUE, check.names = FALSE)
aliases <- aliases[order(aliases$checked, aliases$ecoevo_names), ]
aliases_add <- mutate(
  filter(aliases,
         !is.na(carnegie_names),
         include_gsheetdb == "Y"),
  Alias =
    paste(ecoevo_names, state.name[match(`State abbreviation`, state.abb)]),
  nonmanual_alias_tomatch =
    paste(carnegie_names, state.name[match(`State abbreviation`, state.abb)]))

carnegie_dat <- rbind(
  carnegie_dat,
  data.frame(check.names = FALSE,
             "Alias" = aliases_add$Alias,
             "Carnegie Institution Name" = aliases_add$carnegie_names,
             "State abbreviation" = aliases_add$`State abbreviation`,
             "2021 Basic Classification" =
               carnegie_dat$`2021 Basic Classification`[
                 match(aliases_add$nonmanual_alias_tomatch, carnegie_dat$Alias)]))


##Write ----
write.csv(carnegie_dat, "./data-raw/carnegiedb_withaliases.csv",
          row.names = FALSE)

