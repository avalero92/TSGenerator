library(testthat)
library(TSGenerator)

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
    Download.VI(
      user = "test", password = "test",
      dataset_id = "test", download_path = fake_path
    ),
    regexp = "La ruta de descarga no existe:.*ruta_que_no_existe_12345"
  )
})

# ------------------------------------------------------------------
# TEST 3: Falta 'reticulate'
# ------------------------------------------------------------------
test_that("Download.VI avisa si falta el paquete 'reticulate'", {
  mock_require <- function(pkg, ...) {
    if (pkg == "reticulate") return(FALSE)
    TRUE
  }

  with_mocked_bindings(
    requireNamespace = mock_require,
    code = {
      expect_error(
        Download.VI(user = "u", password = "p", dataset_id = "d", download_path = tempdir()),
        "Por favor instala el paquete 'reticulate'"
      )
    },
    .package = "base"
  )
})

# ------------------------------------------------------------------
# TEST 4: Ruta de Python inválida
# ------------------------------------------------------------------
test_that("Download.VI falla si ruta_python no existe", {
  fake_python <- file.path(tempdir(), "python_inexistente.exe")
  expect_error(
    Download.VI(
      user = "u", password = "p",
      dataset_id = "d", download_path = tempdir(),
      ruta_python = fake_python
    ),
    regexp = "La ruta de Python especificada no existe.*python_inexistente\\.exe"
  )
})

# ------------------------------------------------------------------
# TEST 5: Módulo 'hda' no encontrado (simulado)
# ------------------------------------------------------------------
test_that("Download.VI falla si no puede importar 'hda'", {
  mock_import <- function(name, ...) {
    if (name == "hda") {
      stop("No module named 'hda'", call. = FALSE)
    }
    structure(list(), class = "python.builtin.module")
  }

  with_mocked_bindings(
    import = mock_import,
    .package = "reticulate",
    code = {
      expect_error(
        Download.VI(
          user = "u", password = "p",
          dataset_id = "d", download_path = tempdir()
        ),
        regexp = "Error general:.*No module named 'hda'"
      )
    }
  )
})

# ------------------------------------------------------------------
# TEST 6: Configuración de cliente falla (simulado)
# ------------------------------------------------------------------
test_that("Download.VI falla si falla la configuración del cliente", {
  mock_hda <- list(
    Configuration = function(...) stop("credenciales inválidas", call. = FALSE),
    Client = function(...) NULL
  )

  with_mocked_bindings(
    import = function(...) mock_hda,
    .package = "reticulate",
    code = {
      expect_error(
        Download.VI(
          user = "bad", password = "bad",
          dataset_id = "d", download_path = tempdir()
        ),
        regexp = "Error general: credenciales inválidas"
      )
    }
  )
})

# ------------------------------------------------------------------
# TEST 7: Búsqueda falla (simulado)
# ------------------------------------------------------------------
test_that("Download.VI maneja errores en la búsqueda", {
  mock_config <- structure(list(), class = "python.config")
  mock_client <- list(
    search = function(...) stop("Timeout en la API", call. = FALSE)
  )
  mock_hda <- list(
    Configuration = function(...) mock_config,
    Client = function(...) mock_client
  )

  with_mocked_bindings(
    import = function(...) mock_hda,
    .package = "reticulate",
    code = {
      expect_error(
        Download.VI(
          user = "u", password = "p",
          dataset_id = "d", download_path = tempdir()
        ),
        regexp = "Error general:.*Timeout en la API"
      )
    }
  )
})

# ------------------------------------------------------------------
# TEST 8: No hay resultados
# ------------------------------------------------------------------
test_that("Download.VI devuelve lista correcta si no hay resultados", {
  mock_matches <- structure(list(), class = "python.list")
  mock_client <- list(search = function(...) mock_matches)
  mock_config <- structure(list(), class = "python.config")
  mock_hda <- list(
    Configuration = function(...) mock_config,
    Client = function(...) mock_client
  )

  with_mocked_bindings(
    import = function(...) mock_hda,
    .package = "reticulate",
    code = {
      result <- Download.VI(
        user = "u", password = "p",
        dataset_id = "empty", download_path = tempdir()
      )
      expect_false(result$success)
      expect_equal(result$count, 0)
      expect_s3_class(result$matches, "python.list")
    }
  )
})

# ------------------------------------------------------------------
# TEST 9: Descarga falla (simulado)
# ------------------------------------------------------------------
test_that("Download.VI maneja errores en descarga", {
  mock_download <- function(path) stop("Error de Red", call. = FALSE)
  mock_matches <- structure(list(download = mock_download), class = "python.list")
  mock_client <- list(search = function(...) mock_matches)
  mock_config <- structure(list(), class = "python.config")
  mock_hda <- list(
    Configuration = function(...) mock_config,
    Client = function(...) mock_client
  )

  with_mocked_bindings(
    import = function(...) mock_hda,
    .package = "reticulate",
    code = {
      expect_warning(
        Download.VI(
          user = "u", password = "p",
          dataset_id = "d", download_path = tempdir()
        ),
        "Error en la descarga: Error de Red"
      )
      # Verificar que success = FALSE
      result <- suppressWarnings(
        Download.VI(user = "u", password = "p", dataset_id = "d", download_path = tempdir())
      )
      expect_false(result$success)
    }
  )
})

# ------------------------------------------------------------------
# TEST 10: Flujo completo (mock exitoso)
# ------------------------------------------------------------------
test_that("Download.VI devuelve estructura correcta en éxito simulado", {
  mock_download <- function(path) TRUE
  mock_matches <- structure(
    list(download = mock_download),
    class = "python.list"
  )
  mock_client <- list(search = function(...) mock_matches)
  mock_config <- structure(list(), class = "python.config")
  mock_hda <- list(
    Configuration = function(...) mock_config,
    Client = function(...) mock_client
  )

  with_mocked_bindings(
    import = function(...) mock_hda,
    .package = "reticulate",
    code = {
      result <- Download.VI(
        user = "u", password = "p",
        dataset_id = "d", download_path = tempdir()
      )
      expect_true(result$success)
      expect_equal(result$count, 1)
      expect_s3_class(result$matches, "python.list")
    }
  )
})

# ------------------------------------------------------------------
# TEST 11: Mensajes informativos
# ------------------------------------------------------------------
test_that("Download.VI muestra mensajes esperados", {
  mock_matches <- structure(list(), class = "python.list")
  mock_client <- list(search = function(...) mock_matches)
  mock_config <- structure(list(), class = "python.config")
  mock_hda <- list(
    Configuration = function(...) mock_config,
    Client = function(...) mock_client
  )

  with_mocked_bindings(
    import = function(...) mock_hda,
    .package = "reticulate",
    code = {
      expect_message(
        Download.VI(
          user = "u", password = "p",
          dataset_id = "d", download_path = tempdir()
        ),
        "Iniciando búsqueda"
      )
      expect_message(
        Download.VI(
          user = "u", password = "p",
          dataset_id = "d", download_path = tempdir()
        ),
        "Se encontraron 0 resultados"
      )
    }
  )
})
