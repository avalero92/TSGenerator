# tests/testthat/test-download-vi.R
library(testthat) library(TSGenerator)
# ------------------------------------------------------------------
# TEST 1: Parámetros obligatorios
# ------------------------------------------------------------------
test_that("Download.VI falla si faltan parámetros obligatorios", {
  expect_error(
    Download.VI(dataset_id = "test", download_path = tempdir()),
    "Los parámetros user, password, dataset_id y download_path son obligatorios"
    )

expect_error(
  Download.VI(user = "u", password = "p", download_path = tempdir()),
  "Los parámetros user, password, dataset_id y download_path son obligatorios"
  )

expect_error(
  Download.VI(user = "u", password = "p", dataset_id = "d"),
  "Los parámetros user, password, dataset_id y download_path son obligatorios"
  )
})

# ------------------------------------------------------------------
# TEST 2: Ruta de descarga inválida
# ------------------------------------------------------------------

test_that("Download.VI falla si la ruta de descarga no existe", {
  fake_path <- file.path(tempdir(), "ruta_que_no_existe_12345")

expect_error(
  Download.VI( user = "test", password = "test",
               dataset_id = "test", download_path = fake_path
               ),
  paste("La ruta de descarga no existe:", fake_path)
  )
})

# ------------------------------------------------------------------
# TEST 3: Falta 'reticulate'
# ------------------------------------------------------------------
test_that("Download.VI avisa si falta el paquete 'reticulate'", {
  # Simular que reticulate no está instalado
  mock_require <- function(pkg, ...) {
    if (pkg == "reticulate") return(FALSE)
    TRUE
    }
   with_mocked_bindings(
     requireNamespace = mock_require,
     { expect_error( Download.VI( user = "u", password = "p",
                                  dataset_id = "d", download_path = tempdir() ), "Por favor instala el paquete 'reticulate'" ) }, .package = "base" ) })

# ------------------------------------------------------------------
# TEST 4: Ruta de Python inválida
# ------------------------------------------------------------------
test_that("Download.VI falta si ruta_python no existe", {
  fake_python <- file.path(tempdir(), "python_inexistente.exe")

  expect_error( Download.VI(
    user = "u", password = "p",
    dataset_id = "d", download_path = tempdir(),
    ruta_python = fake_python ),
    paste("La ruta de Python especificada no existe:", fake_python) )
  })

# ------------------------------------------------------------------
# TEST 5: Módulo 'hda' no encontrado (simulado)
# ------------------------------------------------------------------

test_that("Download.VI falta si no puede importar 'hda'", {
  # Mock de reticulate::import que lanza error
  mock_import <- function(name) {
    if (name == "hda") {
      stop("no module named 'hda'")
  }
  NULL
  }

with_mocked_bindings(
  import = mock_import,
  {
    expect_error( Download.VI(
      user = "u", password = "p",
      dataset_id = "d", download_path = tempdir()
      ),
      "Error al importar el módulo 'hda'"
      )
    },
  .package = "reticulate"
  )
})

# ------------------------------------------------------------------
# TEST 6: Configuración de cliente falla (simulado)
# ------------------------------------------------------------------

test_that("Download.VI falta si falla la configuración del cliente", {
  mock_hda <- list(
    Configuration= function(...) stop ("credenciales inválidas"),
    Client = function(...) NULL
    )

with_mocked_bindings(
  import = function(...) mock_hda,
  {
    expect_error( Download.VI(
      user = "bad", password = "bad",
      dataset_id = "d", download_path = tempdir()
      ),
      "Error en la configuración del cliente HDA"
      ) },
  .package = "reticulate"
  )
})

# ------------------------------------------------------------------
# TEST 7: Búsqueda falla (simulado)
# ------------------------------------------------------------------

test_that("Download.VI maneja errores en la búsqueda", {
  mock_client <- list(
    search= function(...) stop("Timeout en la API")
    )

  mock_hda <- list(
  Configuration = function(...) NULL,
  Client = function(...) mock_client
  )

  with_mocked_bindings(
    import = function(...) mock_hda,
    {
      expect_error(
        Download.VI( user = "u", password = "p",
                     dataset_id = "d", download_path = tempdir()
                     ),
        "Error en la búsqueda"
        ) },
    .package = "reticulate"
    )
  })

# ------------------------------------------------------------------
# TEST 8: No hay resultados
# ------------------------------------------------------------------
test_that("Download.VI devuelve lista correcta si no hay resultados", {
  mock_matches <- structure(list(), class="python.list" )
  mock_client <- list(search = function(...) mock_matches)
  mock_hda <- list(Configuration = function(...) NULL, Client= function(...) mock_client)


with_mocked_bindings(
  import = function(...) mock_hda,
  {
    result <- Download.VI(
      user ="u" , password ="p" ,
      dataset_id = "empty", download_path = tempdir()
      )

    expect_false(result$success)
    expect_equal(result$count, 0)
    expect_s3_class(result$matches, "python.list")
    },
  .package = "reticulate"
  )
})

# ------------------------------------------------------------------
# TEST 9: Descarga falla (simulado)
# ------------------------------------------------------------------

test_that("Download.VI maneja errores en descarga", {
  mock_download <- function(path) stop("Error de Red")
  mock_matches <- structure(list(download = mock_download), class="python.list" )
  mock_client <- list(search = function(...) mock_matches)
  mock_hda <- list(Configuration = function(...) NULL, Client= function(...) mock_client)

with_mocked_bindings(
  import = function(...) mock_hda,
  {
    expect_message(
      Download.VI( user = "u", password = "p",
      dataset_id = "d", download_path = tempdir()
      ),
      "Error en la descarga" ) }, .package = "reticulate"
  )
})

# ------------------------------------------------------------------
# TEST 10: Flujo completo (mock exitoso)
# ------------------------------------------------------------------
test_that("Download.VI devuelve estructura correcta en éxito simulado", {
  mock_download <- function(path) TRUE
  mock_matches <- structure(
    list(download = mock_download),
    class="python.list"
    )

  mock_client <- list(search = function(...) mock_matches)
  mock_hda <-  list(Configuration = function(...) NULL, Client= function(...) mock_client)

  with_mocked_bindings(
    import = function(...) mock_hda, {
      result <- Download.VI(
        user="u" , password="p",
        dataset_id="d", download_path= tempdir()
        )
  expect_true(result$success)
  expect_equal(result$count, 1)
  expect_s3_class(result$matches, "python.list")
  },
  .package = "reticulate"
  )
  })

# ------------------------------------------------------------------
# TEST 11: Mensajes informativos
# ------------------------------------------------------------------
test_that("Download.VI muestra mensajes esperados", {
  mock_matches <- structure(list(), class="python.list" )
  mock_client <- list(search = function(...) mock_matches)
  mock_hda <- list(Configuration = function(...) NULL, Client= function(...) mock_client)

  with_mocked_bindings(
    import = function(...) mock_hda,
    {
      expect_message(
        Download.VI(
          user = "u", password = "p",
          dataset_id = "d", download_path = tempdir()
          ),
        "Iniciando búsqueda"
        )

      expect_message(
        Download.VI(
          user = "u", password = "p", dataset_id = "d",
          download_path = tempdir()
          ),
          "Se encontraron 0 resultados"
        )
      },
    .package = "reticulate"
    )
  })
