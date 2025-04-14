read_data <- function(dir) {
    dt <- readstata13::read.dta13(dir, generate.factors=TRUE)
    ## Convert 0/1 to logical
    for (j in seq_len(ncol(dt))) {
        if(identical(sort(unique(dt[, j])), c(0L, 1L)))
            dt[, j] <- as.logical(dt[, j])
    }
    attr(dt, "expansion.fields") <- NULL
    dt
}

export_est_binary <- function(a) {
  # Use alternative output if available:
  est <- if (is.null(a$est_o)) a$est_f else a$est_o
  cb  <- if (is.null(a$cb_o)) a$cb_f else a$cb_o
  
  # If there are fewer than 3 rows, assume binary treatment and use the first row
  if(nrow(est) < 3){
    row <- 1
    row_cb <- 1
  } else {
    row <- (seq.int(nrow(est)/3)-1)*3+1
    row_cb <- (seq.int(nrow(est)/3)-1)*2+1
  }
  
  d <- data.frame(
    treatment = row.names(est[row, , drop = FALSE]),
    reg_est   = est[row, 1],
    own_est   = est[row, 2],
    ate_est   = est[row, 3],
    ew_est    = if(ncol(est) >= 4) est[row, 4] else NA,
    cw_est    = if(ncol(est) >= 5) est[row, 5] else NA,
    bias_est  = if(nrow(cb) >= row_cb+1) cb[row_cb, 2] else NA,
    reg_se    = if(nrow(est) >= row+1) est[row+1, 1] else NA,
    own_se    = if(nrow(est) >= row+1) est[row+1, 2] else NA,
    bias_se   = if(nrow(cb) >= row_cb+1) cb[row_cb+1, 2] else NA
  )
  rownames(d) <- NULL
  return(d)
}


write_rd_table <- function(a, col=2, file, digits=3, star=FALSE) {
    T1 <- a$est_f
    T2 <- a$cb_f
    ns <- c(a$n_f, NA, NA, NA, NA)
    ks <- c(a$k_f, NA, NA, NA, NA)
    tests <- rbind(statistic=c(a$t_f$W, a$t_f$LM, NA, NA, NA),
                   DOF=c(a$t_f[[2]], a$t_f[[5]], NA, NA, NA),
                   "p-value"=c(a$t_f[[3]], a$t_f[[6]], NA, NA, NA))

    if (col==2) {
        T1 <- cbind(T1, a$est_o)
        T2 <- cbind(T2, a$cb_o)
        tests <- cbind(tests, rbind(statistic=c(a$t_o$W, a$t_o$LM, NA, NA, NA),
                                    DOF=c(a$t_o[[2]], a$t_o[[5]], NA, NA, NA),
                                    "p-value"=c(a$t_o[[3]], a$t_o[[6]], NA,
                                                NA, NA)))
        ks <- c(ks, c(a$k_o, NA, NA, NA, NA))
        ns <- c(ns, c(a$n_o, NA, NA, NA, NA))
    }
    pop <- (seq.int(nrow(T1)/3)-1)*3+1
    odd <- (seq.int(nrow(T2)/2)-1)*2+1

    s_level <-  if (star) 0.05 else 0
    star1 <- (abs(T2[odd, ]/T2[odd+1, ]) > stats::qnorm(1-s_level)) +
        (abs(T2[odd, ]/T2[odd+1, ]) > stats::qnorm(1-s_level/2)) +
        (abs(T2[odd, ]/T2[odd+1, ]) > stats::qnorm(1-s_level/10))
    star1[is.na(star1)] <- 0L

    format_int <- function(x) format(x, nsmall=0, big.mark=",")
    format_re <- function(x) formatC(x, format="f", digits=digits)
    t1 <- ifelse(is.na(T1), "", format_re(T1))
    t1[pop+1, ] <- paste0("(", paste(t1[pop+1, ], sep=","), ")")
    t1[pop+2, ] <- paste0("[", paste(t1[pop+2, ], sep=","), "]")
    t1 <- ifelse(t1=="()", "", t1)
    t1 <- ifelse(t1=="[]", "", t1)
    t1[pop, ] <- ifelse(star1>2, paste0(t1[pop, ], "$^{***}$"), t1[pop, ])
    t1[pop, ] <- ifelse(star1==2, paste0(t1[pop, ], "$^{**}$"), t1[pop, ])
    t1[pop, ] <- ifelse(star1==1, paste0(t1[pop, ], "$^{*}$"), t1[pop, ])

    t1 <- rbind(t1,
                ifelse(is.na(ks), "", paste0("\\multicolumn{1}{r}{",
                                             format_int(ks), "}")),
                ifelse(is.na(ns), "", paste0("\\multicolumn{1}{r}{",
                                             format_int(ns), "}")))


    t2 <- ifelse(is.na(T2), "", format_re(T2))
    t2[odd+1, ] <- paste0("(", paste(t2[odd+1, ], sep=","), ")")
    t2 <- ifelse(t2=="()", "", t2)

    rownames(t2)[odd+1] <- ""
    rownames(t1)[c(pop+1, pop+2)] <- ""

    rownames(t1)[NROW(t1)-1] <- "Number of controls"
    rownames(t1)[NROW(t1)] <- "Sample size"

    t1 <- cbind(rownames(t1), t1)
    t2 <- cbind(rownames(t2), t2)

    t1 <- tidyr::unite(data=as.data.frame(t1), col="z", sep = " & ")
    t1 <- unlist(tidyr::unite(data=cbind(t1, c(rep("\\\\", nrow(t1)-1), "")),
                              col="z", sep=""))
    t2 <- tidyr::unite(data=as.data.frame(t2), col="z", sep = " & ")
    t2 <- unlist(tidyr::unite(data=cbind(t2, c(rep("\\\\", nrow(t2)-1), "")),
                              col="z", sep=""))
    write(unname(t1), file=paste0(file, "A.tex"))
    write(unname(t2), file=paste0(file, "B.tex"))
}

## Worst-case contamination bias
decomposition2 <- function(Y, X, Zm) {
    K <- nlevels(X)-1
    L <- ncol(Zm)

    Xf <- outer(X, levels(X), `==`)  # full X matrix
    Xm <- outer(X, levels(X)[-1], `==`) + 0  # X matrix
    ri <- lm(Y~0+Zm+Xm:Zm)                   # interactions
    gamma <- ri$coefficients[-seq_len(ncol(Zm))] # CATEs
    psi_gamma <- ((ri$residuals*model.matrix(ri)) %*%
                      solve(crossprod(model.matrix(ri))))[, -seq_len(ncol(Zm))]
    rd <- lm(model.matrix(lm(Y~0+Zm:Xm))~0+Xm+Zm) # delta matrix
    ## Sort each set of columns by size
    sorts <- function(v, decreasing=FALSE) {
        f <- function(x) sort(x, index.return=TRUE, decreasing=decreasing)$ix

        as.vector(apply(matrix(v, nrow=L), 2, f)) + rep((0:(K-1))*L, each=L)
    }
    gi <- sorts(gamma) # sort (gamma_1,..., gamma_K)
    gd <- sorts(gamma, TRUE)
    est <- se <- matrix(nrow=K, ncol=(3+2*(K-1)))
    cate <- matrix(gamma, ncol=K)
    ps <- lm(Xf~0+Zm)$coefficients # propensity scores

    fw <- colSums(Zm)
    wgt_cw <- 1/rowSums((1/ps) %*% diag(colMeans(Xf)*(1-colMeans(Xf))))
    wgt_cw <- wgt_cw/weighted.mean(wgt_cw, w=fw)
    wgt_ew <- ps[, 1]*ps[, -1]/(ps[, 1]+ps[, -1])
    wgt_ew <- wgt_ew %*% diag(sum(fw)/colSums(wgt_ew*fw))
    wgt_own <- wgt_ew*NA
    wgt_cb <- sd_cb <- list()
    sd_own <- vector(length=K)
    ## Standard errors
    sd_cate <- sqrt(diag(cov.wt(cate, fw)$cov)-
                        cov.wt(matrix(colSums(psi_gamma^2), ncol=K), fw)$center)

    for (k in seq_len(K)) {
        ddX <- lm(Xm[, k]~0+Xm[, -k]+Zm)$residuals # ddot(X)
        deltak <- rd$coefficients[k, ] # (delta_{1,k}, ..., delta{K, k})
        psi_deltak <- ddX*rd$residuals / sum(ddX^2)
        M <- kronecker(diag(K), rep(1, L))
        psi <- psi_deltak %*% (gamma*M) + psi_gamma %*% (deltak*M)
        estk <- drop(gamma %*% (deltak*M)) # each of K components
        di <- sorts(deltak) # sort each vector (delta_{1,k}, ..., delta{K, k})
        wgt_own[, k] <- matrix(deltak, ncol=K)[, k]*sum(fw)/fw
        wgt_cb[[k]] <- matrix(deltak, ncol=K)[, -k]*sum(fw)/fw
        wgtk <- matrix(deltak, ncol=K)*sum(fw)/fw
        var_wgtk <- diag(cov.wt(wgtk, fw)$cov)-
            cov.wt(matrix(colSums(psi_deltak^2), ncol=K), fw)$center
        sd_own[k] <- sqrt(var_wgtk[k])
        sd_cb[[k]] <- sqrt(var_wgtk[-k])
        est[k, ] <- c(sum(estk), estk[k], sum(estk[-k]),
                      drop(gamma[gd] %*% (deltak[di]*M))[-k],
                      drop(gamma[gi] %*% (deltak[di]*M))[-k])

        psimax <- psi_deltak[, di] %*% (gamma[gi]*M) +
            psi_gamma[, gi] %*% (deltak[di]*M)
        psimin <- psi_deltak[, di] %*% (gamma[gd]*M) +
            psi_gamma[, gd] %*% (deltak[di]*M)
        se[k, ] <- sqrt(c(sum(rowSums(psi)^2), sum(psi[, k]^2),
                          sum(rowSums(psi[, -k, drop=FALSE])^2),
                          colSums(psimin[, -k, drop=FALSE]^2),
                          colSums(psimax[, -k, drop=FALSE]^2)))
    }

    rownames(se) <- rep("se", K)
    rownames(est) <- colnames(cate) <- levels(X)[-1]
    colnames(est) <- c("beta", "own", "cont. bias", rep("minbias", K-1),
                       rep("maxbias", K-1))
    tbl <- rbind(est, se)[rep(seq_len(nrow(est)), each=2) + c(0, nrow(est)), ]
    list(table=tbl, wgt_cw=wgt_cw, fw=fw, wgt_ew=wgt_ew,
         wgt_own=wgt_own, wgt_cb=wgt_cb, cate=cate, ps=ps,
         sd_cate=sd_cate, sd_own=sd_own, sd_cb=sd_cb)
}
