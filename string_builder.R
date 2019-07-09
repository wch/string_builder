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
string_builder_file <- function() {
  conn <- file(open="w+b")
  bytes <- 0L

  list(
    add = function(...) {
      new_content <- paste0(unlist(list(...)), collapse = "")
      raw <- charToRaw(new_content)
      bytes <<- bytes + length(raw)
      writeBin(raw, conn)
    },
    get = function() {
      flush(conn)
      # For testing purposes, close after reading once. Otherwise we'll end up
      # with lots of open connections.
      on.exit(close(conn))

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
  len <- 0L

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


# Same as _bracket, but it uses `self` to refer to the environment. The use of
# self to refer to the object results in extra copies because of how `$<-`
# works.
string_builder_bracket_self <- function() {
  self <- environment()
  buffer <- character(0)
  len <- 0

  list(
    add = function(...) {
      new_content <- unlist(list(...))
      new_length <- length(new_content)
      self$buffer[len + seq_len(new_length)] <<- new_content
      self$len <<- len + new_length
    },
    get = function() {
      paste(self$buffer, collapse = "")
    }
  )
}


string_builder_bracket_self2 <- function() {
  self <- new.env(parent = emptyenv())
  self$buffer <- character(0)
  self$len <- 0

  list(
    add = function(...) {
      new_content <- unlist(list(...))
      new_length <- length(new_content)
      self$buffer[self$len + seq_len(new_length)] <<- new_content
      self$len <<- self$len + new_length
    },
    get = function() {
      paste(self$buffer, collapse = "")
    }
  )
}


# Same as _bracket, but it uses `self` to refer to the environment. The use of
# self to refer to the object results in extra copies because of how `$<-`
# works.
string_builder_doubler <- function() {
  buffer <- character(20)
  count <- 0L  # Current number of items

  list(
    add = function(...) {
      new_content <- unlist(list(...))
      new_count <- count + length(new_content)

      # Double in size if needed
      while (new_count > length(buffer)) {
        buffer[length(buffer) * 2] <<- NA_character_
      }
      buffer[count + seq_along(new_content)] <<- new_content
      count <<- new_count
    },
    get = function() {
      paste(buffer[seq_len(count)], collapse = "")
    }
  )
}

string_builder_env <- function() {
  e <- new.env()
  count <- 1L  # Current number of items

  list(
    add = function(...) {
      new_content <- unlist(list(...))
      name <- sprintf("%09d", count)
      e[[name]] <<- new_content
      count <<- count + 1
    },
    get = function() {
      names <- sprintf("%09d", seq_len(count))
      strings <- unlist(lapply(names, function(name) e[[name]]))
      paste(strings, collapse = "")
    }
  )
}



# Note that some care will need to be taken when the input is vectors of strings
# that are different lengths.

string_builders <- list(
  paste   = string_builder_paste,
  c       = string_builder_c,
  file    = string_builder_file,
  bracket = string_builder_bracket,
  bracket_self = string_builder_bracket_self,
  doubler = string_builder_doubler,
  env     = string_builder_env
)



ks <- as.character(rnorm(1e4))

system.time({
  s <- string_builder_bracket()
  for (k in ks) {
    s$add(k)
  }
})
system.time({
  s <- string_builder_bracket_self()
  for (k in ks) {
    s$add(k)
  }
})
system.time({
  s <- string_builder_bracket_self2()
  for (k in ks) {
    s$add(k)
  }
})

# =============================================================================
# Correctness checks
# =============================================================================
# Check that all of the string builders give the same output for a given input.

input_strings <- c("abcd", "", "xyz", "  ", "123", "\n", "")

string_output <- lapply(string_builders, function(string_builder) {
  s <- string_builder()
  for (string in input_strings) {
    # Add the string, along with string vectors, to exercise things a bit.
    s$add(string)
    s$add(" ", string, " ")
    s$add(c("1", "2"), string)
  }

  s$get()
})

invisible(lapply(string_output, function(output) {
  stopifnot(identical(string_output[[1]], output))
  output
}))

print(string_output[[1]])


# =============================================================================
# Timings for string builders
# =============================================================================

library(bench)

results <- bench::press(
  # string_builder = string_builders,
  n_string = c(100, 200, 500, 1000, 2000, 5000),
  string_size = c(5),
  {
    input_strings <- vapply(seq_len(n_string), function(n) {
      paste0(sample(letters, string_size, replace = TRUE), collapse = "")
    }, "")

    add_strings <- function(builder_name) {
      s <- string_builders[[builder_name]]()
      for (i in seq_along(input_strings)) {
        s$add(input_strings[i], " ")
      }
      s$get()
      invisible(NULL) # We don't want bench::mark to save the string result
    }

    bench::mark(
      add_strings("bracket"),
      add_strings("bracket_self"),
      add_strings("env"),
      add_strings("c"),
      add_strings("paste"),
      add_strings("file"),
      add_strings("doubler")
    )
  }
)


library(dplyr)

library(scales)
library(ggplot2)
ggplot(results, aes(n_string, median, color = expression)) +
  geom_line(aes(group = expression)) +
  geom_point() +
  scale_y_continuous(
    trans = log2_trans(),
    breaks = trans_breaks("log2", function(x) 2^x, n = 12),
    minor_breaks = NULL,
    labels = trans_format("log2", math_format(2^.x))
  ) +
  scale_x_continuous(
    trans = log10_trans(),
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  )


results$median
#
# benchmark <- function(string_builders, n_strings) {
#   results <- data.frame()
#
#   for (n in n_strings) {
#     cat("n strings=", n, "...", sep = "")
#     # Generate some random input strings
#     input_strings <- vapply(seq_len(n), function(n) {
#       paste0(sample(letters, 500, replace = TRUE), collapse = "")
#     }, "")
#
#     times <- list()
#
#     # Benchmark each string builder for this size
#     for (name in names(string_builders)) {
#       cat(name, " ", sep = "")
#       string_builder <- string_builders[[name]]
#
#       time <- system.time({
#         s <- string_builder()
#         for (i in seq_along(input_strings)) {
#           s$add(input_strings[i], " ")
#         }
#         s$get()
#       })
#
#
#       results <- rbind(
#         results,
#         data.frame(
#           r_version = paste0(R.version$major, ".", R.version$minor),
#           type = name,
#           time = round(time[['elapsed']], 3),
#           n_strings = n
#         )
#       )
#     }
#
#     write.table(results, "results.csv",
#       col.names = FALSE, row.names = FALSE, sep = ","
#     )
#     cat("\n")
#   }
# }
#
# benchmark(string_builders, c(200, 500, 1000, 2000, 5000))
