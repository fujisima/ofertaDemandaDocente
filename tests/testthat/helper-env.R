renviron_path <- Filter(
  file.exists,
  c(".Renviron", "../.Renviron", "../../.Renviron")
)

if (length(renviron_path) > 0) {
  readRenviron(renviron_path[[1]])
}
