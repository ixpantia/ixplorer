context("verificacion")

test_that("Finds file .ixplorer", {
  file <- verify_ixplorer_file()
  expect_true(any(class(file) == "data.frame"))
})

test_that("Detects missing data TOKEN access value", {
  file <- readr::read_csv("../testdata/.ixplorer(api)") %>%
    tidyr::separate(col = V1, into = c("object", "value"), sep = " ")
  expect_true(verify_ixtoken(file) == "There is no ixplorer TOKEN, please use the Authentication gadget")
})

test_that("Detects missing data URL access value", {
  file <- readr::read_csv("../testdata/.ixplorer(url)") %>%
    tidyr::separate(col = V1, into = c("object", "value"), sep = " ")
  expect_true(verify_ixtoken(file) == "There is no ixplorer URL, please use the Authentication gadget")
})

test_that("Detects missing data PROJECT access value", {
  file <- readr::read_csv("../testdata/.ixplorer(project)") %>%
    tidyr::separate(col = V1, into = c("object", "value"), sep = " ")
  expect_true(verify_ixowner(file) == "There is no ixplorer PROJECT name, please use the Authentication gadget")
})

test_that("Detects missing data PROJECT access value", {
  file <- readr::read_csv("../testdata/.ixplorer(project)") %>%
    tidyr::separate(col = V1, into = c("object", "value"), sep = " ")
  expect_true(verify_ixowner(file) == "There is no ixplorer PROJECT name, please use the Authentication gadget")
})

test_that("Detects missing data USER access value", {
  file <- readr::read_csv("../testdata/.ixplorer(user)") %>%
    tidyr::separate(col = V1, into = c("object", "value"), sep = " ")
  expect_true(verify_ixuser(file) == "There is no ixplorer USER, please use the Authentication gadget")
})



# Hay un error o paso para verificar con posiciones de datos
# Si eliminio una linea, la posicion varia y queda mal las condiciones
# dentro de los verify.

# el msj de set_authentication se esta repitiendo. YO habia provocado eso
# adrede porque no hacia un print del msj y tuve que hacerlo explicito
# ahora se esta comportando imprimiendo dos veces.
