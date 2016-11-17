require(R6)

RandomKeyGenerator <- function(len) {
  paste0(sample(x = c(0:9, letters, LETTERS), size = len, replace = T), collapse = "")
}

Node <- R6Class("Node",
  public = list(
    value = NULL,
    key = NULL,
    children = list(),
    parent = NULL,
    initialize = function(value, key = NULL, children = NULL, parents = NULL) {
      self$value <- value
      self$children <- c(self$children, as.list(children))
      if(is.null(key)) {
        self$key <- RandomKeyGenerator(8)
      } else {
        self$key <- key
      }
    },
    getChildren = function() {
      return(self$children)
    }, 
    addChild = function(newChild) {
      self$children <- c(self$children, newChild)
    },
    remChild = function(keyToRemove) {
      for(i in 1:length(children)) {
        if(children[[i]]$key == keyToRemove) {
          children[[i]] <- NULL
          return(invisible(self))
        }
      }
      message("Could not find key")
    },
    getParents = function() {
      return(self$parents)
    },
    setParent = function(newParent) {
      self$parent <- newParent
    } 
  )
)

testnode <- Node$new("()")
testnode$addChild("blub")
