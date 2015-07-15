fa2latex2 = function(f, digits = 2, rowlabels = TRUE, apa = TRUE, short.names = FALSE, 
         cumvar = FALSE, cut = 0, alpha = 0.05, font.size = "scriptsize", 
         heading = "A factor analysis table from the psych package in R", 
         caption = "fa2latex", label = "default") 
{
  if (class(f)[2] == "fa.ci") {
    if (is.null(f$cip)) {
      px <- f$cis$p
    }
    else {
      px <- f$cip
    }
  }
  else {
    px <- NULL
  }
  x <- unclass(f$loadings)
  if (!is.null(f$Phi)) {
    Phi <- f$Phi
  }
  else {
    Phi <- NULL
  }
  nfactors <- ncol(x)
  if (nfactors > 1) {
    if (is.null(Phi)) {
      h2 <- rowSums(x^2)
    }
    else {
      h2 <- diag(x %*% Phi %*% t(x))
    }
  }
  else {
    h2 <- x^2
  }
  u2 <- 1 - h2
  vtotal <- sum(h2 + u2)
  if (cut > 0) 
    x[abs(x) < cut] <- NA
  if (!is.null(f$complexity)) {
    x <- data.frame(x, h2 = h2, u2 = u2, com = f$complexity)
  }
  else {
    x <- data.frame(x, h2 = h2, u2 = u2)
  }
  nvar <- dim(x)[2]
  comment <- paste("% Called in the psych package ", match.call())
  header <- paste("\\begin{", font.size, "} \\begin{table}", 
                  "\\caption{", caption, "}\n\\begin{center}\n\\begin{tabular}", 
                  sep = "")
  header <- c(header, "{l", rep("r", nvar), "}\n")
  if (apa) 
    header <- c(header, "\\multicolumn{", nvar, "}{l}{", 
                heading, "}", "\\cr \n \\hline ")
  if (apa) {
    footer <- paste(" \\hline ")
  }
  footer <- paste(footer, "\n\\end{tabular}\n\\end{center}\n\\label{", 
                  label, "}\n\\end{table} \n\\end{", font.size, "}\n", 
                  sep = "")
  x <- round(x, digits = digits)
  cname <- colnames(x)
  if (short.names) 
    cname <- 1:nvar
  names1 <- paste(cname[1:(nvar - 1)], " & ")
  lastname <- paste(cname[nvar], "\\cr \n")
  if (apa) {
    allnames <- c("Variable  &  ", names1, lastname, " \\hline \n")
  }
  else {
    allnames <- c("  &  ", names1, lastname, "\\cr \n")
  }
  x <- format(x, drop0trailing = FALSE)
{
    if (!is.null(pf) && (cut == 0)) {
      temp <- x[1:nfactors]
      temp[px < alpha] <- paste("\\bf{", temp[px < alpha], 
                                "}", sep = "")
      x[1:nfactors] <- temp
    }
    value <- apply(x, 1, paste, collapse = "  &  ")
    value <- gsub("NA", "  ", value, fixed = TRUE)
    if (rowlabels) 
      value <- {
        paste("  & ", value)
      }
    else {
      paste("  &  ", value)
    }
    values <- paste(value, "\\cr", "\n")
    cat(comment, "\n")
    cat(header)
    cat(allnames)
    cat(values)
    x <- f$loadings
    nvar <- nrow(x)
    if (is.null(Phi)) {
      if (nfactors > 1) {
        vx <- colSums(x^2)
      }
      else {
        vx <- diag(t(x) %*% x)
        vx <- vx * nvar/vtotal
      }
    }
    else {
      vx <- diag(Phi %*% t(x) %*% x)
      vx <- vx * nvar/vtotal
    }
    vx <- round(vx, digits)
    loads <- c("\\hline \\cr SS loadings &", paste(vx, " & ", 
                                                   sep = ""), "\\cr  \n")
    cat(loads)
    if (cumvar) {
      provar <- round(vx/nvar, digits)
      cat("Proportion Var &", paste(provar, "  & ", sep = ""), 
          "\\cr \n")
      if (nfactors > 1) {
        cumvar <- round(cumsum(vx/nvar), digits)
        cumfavar <- round(cumsum(vx/sum(vx)), digits = digits)
        cat("Cumulative Var & ", paste(cumvar, " & ", 
                                       sep = ""), "\\cr \n")
        cat("Cum. factor Var & ", paste(cumsum(vx/sum(vx)), 
                                        "  & ", sep = ""), "\\cr \n")
      }
    }
    if (!is.null(Phi)) {
      cat("\\cr \n            \\hline \\cr \n")
      Phi <- round(Phi, digits)
      phi <- format(Phi, nsmall = digits)
      phi <- apply(phi, 1, paste, collapse = " & ")
      phi <- paste(colnames(x), "  &", phi)
      phi <- paste(phi, "\\cr", "\n")
      cat(phi)
    }
    cat(footer)
  }
}