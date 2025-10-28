test_that("copernicus_login lanza mensaje si no hay credenciales", {
  expect_message(
    copernicus_login(),
    "Introduce tu usuario y contraseña de Copernicus"
    )
  })

test_that("query_hrvpp devuelve data.frame", {
  skip("Requiere conexión a internet")
  result <- query_hrvpp(bbox= c(-3.7, 40.4, -3.6, 40.5),
  start_date="2024-01-01" , end_date="2024-01-10" )
expect_s3_class(result, "data.frame")
})
