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
# reads the file and closes it. Unlike the other string builders, this one
# cannot be used after get() is called.
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
      on.exit(close(conn))
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



# =============================================================================
# Tests
# =============================================================================

# Generate some random input
n_strings <- 2e4
input_strings <- vapply(seq_len(n_strings), function(n) {
  paste0(sample(letters, 5, replace = TRUE), collapse = "")
}, "")

head(input_strings)

# =============================================================================
# Timings for string builders
# =============================================================================

time_paste <- system.time({
  s <- string_builder_paste()
  for (i in seq_along(input_strings)) {
    s$add(input_strings[i], " ")
  }
  res_paste <- s$get()
})


time_c <- system.time({
  s <- string_builder_c()
  for (i in seq_along(input_strings)) {
    s$add(input_strings[i], " ")
  }
  res_c <- s$get()
})


time_bracket <- system.time({
  s <- string_builder_bracket()
  for (i in seq_along(input_strings)) {
    s$add(input_strings[i], " ")
  }
  res_bracket <- s$get()
})

time_bfile <- system.time({
  s <- string_builder_bfile()
  for (i in seq_along(input_strings)) {
    s$add(input_strings[i], " ")
  }
  res_bfile <- s$get()
})

# Save the results for analysis
results <- data.frame(
  r_version = paste0(R.version$major, ".", R.version$minor),
  type = c("paste", "c", "bfile", "bracket"),
  time = c(
    time_paste[['elapsed']],
    time_c[['elapsed']],
    time_bfile[['elapsed']],
    time_bracket[['elapsed']]
  )
)
write.table(results, "results.csv",
  col.names = FALSE, row.names = FALSE, sep = ",", append = TRUE
)

# =============================================================================
# Check that the result looks right
# =============================================================================
nchar(res_paste)
substr(res_paste, 1, 60)

# Make all of the string builders generate identical results.
stopifnot(identical(res_paste, res_c))
stopifnot(identical(res_paste, res_bfile))
stopifnot(identical(res_paste, res_bracket))
