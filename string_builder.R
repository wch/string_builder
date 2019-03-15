# =============================================================================
# Various kinds of string builders
# =============================================================================

# A string builder which keeps a single string in a buffer, and pastes the input
# to the buffer each time add() is called.
string_builder_paste <- function() {
  buffer <- ""

  list(
    add = function(...) {
      new_content <- paste0(unlist(list(...)), collapse = "")
      buffer <<- paste0(buffer, new_content, collapse = "")
    },
    get = function() {
      buffer
    }
  )
}

# A string builder which keeps a vector of strings in a buffer. Each time add()
# is called, it adds the input to the vector with c(). When get() is called, it
# calls paste() on the vector to collapse it to a single string.
string_builder_c <- function() {
  buffer <- character(0)

  list(
    add = function(...) {
      new_content <- unlist(list(...))
      buffer <<- c(buffer, new_content)
    },
    get = function() {
      paste(buffer, collapse = "")
    }
  )
}

# A string builder which uses an anonymous binary file. When get() is called, it
# reads the file.
string_builder_bfile <- function() {
  conn <- file(open="w+b")
  bytes <- 0

  list(
    add = function(...) {
      new_content <- paste0(unlist(list(...)), collapse = "")
      raw <- charToRaw(new_content)
      bytes <<- bytes + length(raw)
      writeBin(raw, conn)
    },
    get = function() {
      flush(conn)
      seek(conn, 0, rw = "read")
      readChar(conn, bytes, useBytes = TRUE)
    }
  )
}


# A string builder which keeps a vector of strings in a buffer. Each time add()
# is called, it adds the input to the vector with `[`-indexing. When get() is
# called, it calls paste() on the vector to collapse it to a single string.
string_builder_bracket <- function() {
  buffer <- character(0)
  len <- 0

  list(
    add = function(...) {
      new_content <- unlist(list(...))
      new_length <- length(new_content)
      buffer[len + seq_len(new_length)] <<- new_content
      len <<- len + new_length
    },
    get = function() {
      paste(buffer, collapse = "")
    }
  )
}

# Note that some care will need to be taken when the input is vectors of strings
# that are different lengths.

string_builders <- list(
  paste   = string_builder_paste,
  c       = string_builder_c,
  bfile   = string_builder_bfile,
  bracket = string_builder_bracket
)





# =============================================================================
# Tests
# =============================================================================


# =============================================================================
# Timings for string builders
# =============================================================================



benchmark <- function(string_builders, n_strings) {
  results <- data.frame()

  for (n in n_strings) {
    cat("n strings=", n, "...", sep = "")
    # Generate some random input strings
    input_strings <- vapply(seq_len(n), function(n) {
      paste0(sample(letters, 5, replace = TRUE), collapse = "")
    }, "")

    times <- list()
    string_result <- list()

    # Benchmark each string builder for this size
    for (name in names(string_builders)) {
      cat(name, " ", sep = "")
      string_builder <- string_builders[[name]]

      time <- system.time({
        s <- string_builder()
        for (i in seq_along(input_strings)) {
          s$add(input_strings[i], " ")
        }
        string_result[[name]] <- s$get()
      })

      ## TODO: Check that results are all the same

      results <- rbind(
        results,
        data.frame(
          r_version = paste0(R.version$major, ".", R.version$minor),
          type = name,
          time = time[['elapsed']],
          n_strings = n
        )
      )
    }

    write.table(results, "results.csv",
      col.names = FALSE, row.names = FALSE, sep = ","
    )
    cat("\n")
  }
}

benchmark(string_builders, c(200, 500, 1000, 2000, 5000, 10000, 20000))
