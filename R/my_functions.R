vcf2df = function(VCF) {
  Site_Info = VCF[, c(1:3)]
  colnames(Site_Info) = c("Chr", "Pos", "Marker")
  VCF_data = VCF[, 10:dim(VCF)[2]]
  
  total_steps = dim(VCF_data)[2]
  
  for (i in 1:total_steps) {
    VCF_data[, i] = ifelse(VCF_data[, i] == "0/0", 0, 
                           ifelse(VCF_data[, i] %in% c("0/1", "1/0"), 1, 
                                  ifelse(VCF_data[, i] == "1/1", 2, NA)))
    shiny::setProgress(value = i / total_steps, message = sprintf("Converting the data... (%d/%d)", i, total_steps))
  }
  
  data = as.data.frame(t(VCF_data))
  colnames(data) = VCF$ID
  rownames(data) = colnames(VCF)[-c(1:9)]
  
  return(data)
}

vcf2Site_Info = function(VCF) {
  Site_Info = VCF[, c(1:3)]
  colnames(Site_Info) = c("Chr", "Pos", "Marker")
  return(Site_Info)
}

Sampleplot = function(rate){
  par(mar = c(3, 4, 0, 1) + 0.1)
  cut = table(cut(rate, breaks = seq(0, 1.0, by = 0.1), include.lowest = T, labels = F))
  hist = hist(rate, freq = T, main = NULL, ylab = "Number of samples",
              xlab = "", ylim = c(0, max(cut)*1.1), col = "#e89978", labels = T)
  return(hist)
}

SNPplot = function(rate){
  par(mar = c(3, 4, 0, 1) + 0.1)
  cut = table(cut(rate, breaks = seq(0, 1.0, by = 0.1), include.lowest = T, labels = F))
  hist = hist(rate, freq = T, main = NULL, ylab = "Number of SNPs",
              xlab = "", ylim = c(0, max(cut)), col = "#e89978", labels = T)
  return(hist)
}

stat2summary = function(stat){
  summary = data.frame("Min" = min(stat), "Max" = max(stat), "Mean" = mean(stat), 
                       "Median" = median(stat), "SD" = sd(stat), "CV" = sd(stat)/mean(stat))
  summary[1:6] = format(round(summary[1:6], 4), nsmall = 4)
  return(summary)
}

size2size = function(size_in_bytes) {
  if (size_in_bytes < 1024) {
    return(paste(size_in_bytes, "B"))
  } else if (size_in_bytes < 1024^2) {
    return(paste(round(size_in_bytes / 1024, 2), "KB"))
  } else if (size_in_bytes < 1024^3) {
    return(paste(round(size_in_bytes / 1024^2, 2), "MB"))
  } else {
    return(paste(round(size_in_bytes / 1024^3, 2), "GB"))
  }
}

popgen2 = function(M, subgroups = NULL){
  withProgress(message = 'Processing data...', value = 0, {
    incProgress(0.05, message = "Checking missing data...")
    hasAllMiss <- colSums(is.na(M)) == nrow(M)
    if(any(hasAllMiss))
      warning("There are some markers with all data missing. These markers were removed from dataset")
    Z <- as.matrix(M[, !hasAllMiss])
    
    incProgress(0.1, message = "Processing data...")
    if(is.null(subgroups))
      subgroups <- 1
    labelSG <- unique(subgroups)
    nSG <- length(labelSG)
    
    incProgress(0.2, message = "Calculating general statistics per sites...")
    general <- g.of.p(Z)
    bygroup <- c("There are no subgroups")
    
    if(nSG > 1){
      incProgress(0.3, message = "Calculating general statistics by group...")
      bygroup <- lapply(labelSG, function(i) g.of.p(Z[subgroups == i, ]))
      names(bygroup) <- labelSG
      pbyg <- sapply(X = as.vector(labelSG), FUN = function(x) bygroup[[x]]$Markers$p)
      
      for(i in 1:nSG){
        fxd <- pbyg[,i] == 1 | pbyg[,i] == 0
        exc <- (pbyg[,i]>0 & apply(pbyg[,-i, drop = FALSE] == 0, 1, all)) |
          (pbyg[,i]<1 & apply(pbyg[,-i, drop = FALSE] == 1, 1, all))
        if(sum(exc) == 0){
          excl <- NA
        } else {
          excl <- colnames(Z)[exc]
        }
        bygroup[[labelSG[i]]]$exclusive <- excl
        if(sum(fxd) == 0){
          fix.g <- NA
        } else {
          fix.g <- colnames(Z)[fxd]
        }
        bygroup[[labelSG[i]]]$fixed <- fix.g
      }
      incProgress(0.35, message = "Calculating F statistics...")
      ngroups <- as.vector(table(subgroups))
      Hig <- matrix(sapply(bygroup, function(x) x$Population["Ho","mean"]), ncol = nSG)
      His <- sapply(bygroup, function(x) x$Markers[,"Ho"])
      Hss <- sapply(bygroup, function(x) x$Markers[,"He"])
      Hsg <- matrix(colMeans(Hss), ncol = nSG)
      Ht <- matrix(general$Markers[,"He"], ncol = 1, dimnames = list(rownames(general$Markers), NULL))
      
      Fstatsg <- F.stats(Hi = Hig, Hs = Hsg, Ht = mean(Ht), ngroups = ngroups)
      Fstatsm <- F.stats(Hi = His, Hs = Hss, Ht = Ht, ngroups = ngroups)
      
      incProgress(0.4, message = "Calculating pairwise F statistics...")
      pw <- combn(x = labelSG, m = 2)
      matFST <- matrix(0, nrow = nSG, ncol = nSG, dimnames = list(labelSG, labelSG))
      
      Fstspw <- round(data.frame("Fis" = numeric(ncol(pw)+1),
                                 "Fst" = numeric(ncol(pw)+1),
                                 "Fit" = numeric(ncol(pw)+1),
                                 row.names = c("All_Pop", paste(pw[1,], pw[2,], sep = "-"))), 4)
      Fstspw[1,] <- Fstatsg
      
      for(i in 1:ncol(pw)){
        sel <- labelSG %in% pw[,i]
        nsbg <- ngroups[sel]
        Hisg <- Hig[,sel, drop=FALSE]
        Hssg <- Hsg[,sel, drop=FALSE]
        Fstspw[i+1,] <- F.stats(Hi = Hisg, Hs = Hssg, Ht = mean(Ht), ngroups = nsbg)
        matFST[pw[1,i], pw[2,i]] <- matFST[pw[2,i], pw[1,i]] <- Fstspw[i+1, 2]						   
      }
      
      Fstats <- list("Genotypes" = Fstspw, "Markers" = Fstatsm)
      
      bygroup <- c(bygroup, list("F.stats" = Fstats))
    }
    incProgress(0.5, message = "Finished processing data...")
    
    out <- list("whole" = general, "bygroup" = bygroup)
    return(out)
  })
}

g.of.p = function(M){
  m=ncol(M)
  g=nrow(M)
  
  p = colMeans(M, na.rm = T)/2
  q = 1-p
  Minor = pmin(p, q)
  Major = pmax(p, q)
  Hesp = 2*p*q
  Hobs = colMeans(M == 1, na.rm = T)
  Dg = 1- p^2 - q^2
  PIC = 1-(p^2 + q^2) - (2*p^2*q^2)
  c0 = colSums(M == 0, na.rm = T)
  c1 = colSums(M == 1, na.rm = T)
  c2 = colSums(M == 2, na.rm = T)
  pi = ((2*c0 + c1)/2) * ((2*c2 + c1)/2) / choose(c0+c1+c2, 2)
  propMiss = colSums(is.na(M))/g
  counts = matrix(NA, nrow = ncol(M), ncol = 3, dimnames = list(colnames(M), c(0,1,2)))
  for(i in 1:3){
    counts[,i] = colSums(M == colnames(counts)[i], na.rm = T)
  }
  hwetest = chiS(counts = counts)																					
  markers = cbind(round(cbind("Minor" = Minor, "Major" = Major, "He" = Hesp, "Ho" = Hobs, "Nei" = Dg, PIC, "Pi" = pi, "Miss" = propMiss), 4),
                  hwetest)
  markers[is.nan(markers)] = NA
  markers = as.data.frame(markers)
  
  Hg.obs = round(rowMeans(M == 1, na.rm = TRUE), 4)
  Fi = round(inbreeding.fun(mat = M, p = p), 4)
  
  genotypes = cbind("Ho" = Hg.obs, "Fi" = Fi)
  
  meanMrk = colMeans(markers, na.rm = TRUE)
  rangeMrk = t(apply(X = markers, MARGIN = 2, FUN = function(x) range(x, na.rm = TRUE)))
  
  meanGen = colMeans(genotypes, na.rm = TRUE)
  rangeGen = t(apply(X = genotypes, MARGIN = 2, FUN = function(x) range(x, na.rm = TRUE)))
  
  population = round(rbind(cbind(meanMrk, rangeMrk)[c(5,6,7,3),], cbind(meanGen, rangeGen)), 4)
  rownames(population) = c(rownames(population)[1:5], "F")
  colnames(population) = c("mean", "lower", "upper")
  
  Ne = (1/(2*mean(Fi)))*g
  Va = sum(2*p*q)
  Vd = sum((2*p*q)^2)
  variance = t(round(data.frame(Ne, Va, Vd, "number of genotypes" = g, "number of markers" = m),4))
  colnames(variance) = ("estimate")
  
  average = list("Markers" = markers, "Genotypes" = genotypes, "Population" = population, "Variability" = variance)
  return(average)
}

F.stats = function(Hi, Hs, Ht, ngroups){
  n.harm = matrix(ngroups/sum(ngroups), nrow = 1)
  
  if(nrow(Hi) > 1){
    n.harm = n.harm[rep(1, nrow(Hi)),]
  }
  
  Hs.pop = rowSums(Hs * n.harm)
  Hi.pop = rowSums(Hi * n.harm)
  
  Fis.pop = (Hs.pop - Hi.pop)/Hs.pop
  Fst.pop = (Ht - Hs.pop)/Ht
  Fit.pop = (Ht - Hi.pop)/Ht
  FST.pop = round(data.frame("Fis" = Fis.pop, "Fst" = Fst.pop, "Fit" = Fit.pop), 4) 
  rownames(FST.pop) = rownames(Ht)
  return(FST.pop)
}

inbreeding.fun = function(mat, p){
  nOHom = rowSums(mat != 1, na.rm = T)
  nEHom = 1 - (2* p *(1-p))
  EH = as.vector(round(nEHom %*%  t(!is.na(mat))))
  Fi = round((nOHom - EH)/(rowSums(!is.na(mat)) - EH), 4)
  return(Fi)
}

chiS = function(counts){
  p = ((2 * counts[,"2"]) + counts[,"1"])/(2*rowSums(counts))
  
  Expfr = Vectorize(FUN = function(p){
    return(c("0" = (1-p)**2, "1" = 2*p*(1-p), "2" = p**2))
  })
  
  E = t(Expfr(p)) * rowSums(counts)
  
  sumChi = rowSums((E - counts)^2/E)
  pvalue = pchisq(sumChi, 1, lower.tail = FALSE)
  resSQ = cbind("chiSq" = round(sumChi, 4), "pval" = pvalue)
  resSQ[is.na(resSQ)] = 0
  return(resSQ)
}

GDsiteplot = function(data, y_axis, y_label){
  for (i in 1:length(unique(data$Chr))) {
    if (i>1){
      end = max(data[data$Chr == i-1, 2])
      data[data$Chr == i, 2] = data[data$Chr == i, 2] + end
    }
  }
  Chr_axis = data %>% group_by(Chr) %>% summarize(center = (max(Pos) + min(Pos)) / 2)
  plot = ggplot(data, aes(x = Pos, y = get(y_axis))) +
    geom_point(aes(color = as.factor(Chr)), alpha = 0.6, size = 0.8) +
    scale_color_manual(values = rep(c("#A4BE7B", "#dfaa52"), 50)) +
    scale_x_continuous(label = Chr_axis$Chr, breaks = Chr_axis$center, expand = c(0.01, 0)) +
    scale_y_continuous(expand = c(0.01, 0)) +
    theme_classic() +
    xlab("Chromosome") +
    ylab(y_label) +
    theme(legend.position = "none",
          panel.border = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 13),
          axis.title.y = element_text(size = 18)
    )
  return(plot)
}

GDgroupplot = function(data, y_axis, y_label){
  data = data[-1,]
  data2 = data.frame(
    x = row.names(data),
    y = data[, which(colnames(data) == y_axis)]
  )
  plot = ggplot(data2, aes(x = x, y = y, color = x)) +
    geom_segment(aes(x = x, xend = x, y = 0, yend = y), color = "grey50") +
    geom_point(size = 7) +
    theme_classic() +
    xlab("") +
    ylab(y_label) +
    scale_y_continuous(expand = c(0.025, 0)) +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 13),
      axis.title.y = element_text(size = 18)
    )
  return(plot)
}

chromosome = function(chr) {
  num = as.numeric(gsub("\\D", "", chr))
  if (num < 10) {
    return(paste0("Chr0", num))
  } else {
    return(paste0("Chr", num))
  }
}

generateCircosPlot = function(Chr_Info, SW_data, pdf_path, Track3, Track4, Track5, Track6) {
  pdf(pdf_path, width = 10, height = 10)
  circos.clear()
  withProgress(message = "Processing Data", value = 0, {
    track = 2+(!is.null(Track3))*1+(!is.null(Track4))*1+(!is.null(Track5))*1+(!is.null(Track6))*1
    shiny::setProgress(value = 1 / track, message = sprintf("Processing Track %d of %d", 1, track))
    # Track 1 & 2
    Chr = length(Chr_Info[,1])
    gap = c(rep(1, Chr-1), 8)
    circos.par("start.degree" = 90, "track.height" = 0.1, "gap.degree" = gap)
    circos.initializeWithIdeogram(Chr_Info, plotType = c("labels"), labels.cex = 0.000000000001)
    brk = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30)*10^7
    circos.track(track.index = get.current.track.index(), panel.fun = function(x, y) {
      circos.axis(h = "top", major.at = brk, labels = round(brk/10^6, 1), labels.cex = 0.7, 
                  col = "grey", labels.col = "grey50", lwd = 0.8, labels.facing = "clockwise")}, bg.border = NA)
    shiny::setProgress(value = 2 / track, message = sprintf("Processing Track %d of %d", 2, track))
    circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
      circos.text(CELL_META$xcenter, CELL_META$ycenter, CELL_META$sector.index, 
                  facing = "inside", niceFacing = T, col = "grey30", cex = 0.9)
    }, bg.col = rep(c("#EEE3CB", "#D7C0AE"), 20), bg.border = NA, track.height = 0.05)
    # Track 3
    if (!is.null(Track3)){
      shiny::setProgress(value = 3 / track, message = sprintf("Processing Track %d of %d", 3, track))
      Track3 = SW_data[, c(1:3, which(colnames(SW_data) == Track3))]
      top = quantile(Track3[,4], probs = 0.99)
      circos.genomicTrack(Track3, panel.fun = function(region, value, ...) {
        circos.genomicLines(region, value, type = "h", lwd = ifelse(value[[1]]>=top, 0.75, 0.55), col = ifelse(value[[1]]>=top, "#ff7878", "#A4BE7B"))
      }, bg.border = NA, ylim = c(0, max(Track3[,4])+0.001), track.height = 0.13)
    }
    # Track 4
    if (!is.null(Track4)){
      shiny::setProgress(value = 4 / track, message = sprintf("Processing Track %d of %d", 4, track))
      Track4 = SW_data[,c(1:3, which(colnames(SW_data) == Track4))]
      top = quantile(Track4[,4], probs = 0.99)
      circos.genomicTrack(Track4, panel.fun = function(region, value, ...) {
        circos.genomicLines(region, value, type = "h", lwd = ifelse(value[[1]]>=top, 0.7, 0.5), col = ifelse(value[[1]]>=top, "#ff7878", "#7ba4be"))
      }, bg.border = NA, ylim = c(0, max(Track4[,4])+0.001), track.height = 0.13)
    }
    # Track 5
    if (!is.null(Track5)){
      shiny::setProgress(value = 5 / track, message = sprintf("Processing Track %d of %d", 5, track))
      Track5 = SW_data[,c(1:3, which(colnames(SW_data) == Track5))]
      top = quantile(Track5[,4], probs = 0.99)
      circos.genomicTrack(Track5, panel.fun = function(region, value, ...) {
        circos.genomicLines(region, value, type = "h", lwd = ifelse(value[[1]]>=top, 0.65, 0.45), col = ifelse(value[[1]]>=top, "#ff7878", "#b695cc"))
      }, bg.border = NA, ylim = c(0, max(Track5[,4])+0.001), track.height = 0.13)
    }
    # Track 6
    if (!is.null(Track6)){
      shiny::setProgress(value = 6 / track, message = sprintf("Processing Track %d of %d", 6, track))
      Track6 = SW_data[,c(1:3, which(colnames(SW_data) == Track6))]
      top = quantile(Track6[,4], probs = 0.99)
      circos.genomicTrack(Track6, panel.fun = function(region, value, ...) {
        circos.genomicLines(region, value, type = "h", lwd = ifelse(value[[1]]>=top, 0.55, 0.4), col = ifelse(value[[1]]>=top, "#ff7878", "#e3907c"))
      }, bg.border = NA, ylim = c(0, max(Track6[,4])+0.001), track.height = 0.13)
    }
  })
  dev.off()
}

core.set = function(data.set, coverage = coverage, difference = difference){
  counts = data.set
  nc <- ncol(data.set)
  nr <- nrow(data.set)
  difference <- difference
  coverage1 <- NULL
  coverage.table <- NULL
  rnames <- rownames(data.set)
  cnames <- colnames(data.set)
  
  var.num <- vector()
  ide.num <- rep(0, nr)
  var.num <- apply(data.set, 1, function(x){length(unique(x[!is.na(x)]))})
  mpe<-sum(var.num - 1)
  
  result <- NULL
  result.idx <- NULL
  overlap.score <- function(x){
    tmp1 <- x
    tmp1.table <- table(tmp1)
    idx <- match(tmp1, names(tmp1.table))
    dummy <- tmp1.table[idx]
    na.idx <- which(is.na(idx))
    if (length(na.idx) > 0){
      dummy[na.idx] <- 0
    }
    dummy
  }
  prenum <- 0
  step0 <- NULL
  step0 <- data.frame(apply(counts, 1, overlap.score))
  rownames(step0) <- cnames
  
  for (idx in 1:mpe){
    withProgress(message = paste("Running: iteration ", idx, "..."), value = NULL, {
      not.na.counts <- apply(counts, 2, function(x){
        length(which(!is.na(x)))
      })
      rnames = rownames(counts)
      cnames = colnames(counts)
      candidate <- which(not.na.counts == max(not.na.counts))
      if (length(candidate) == 1){
        step01 <- step0[candidate,]
        overlap <- mean(step01[!is.na(step01)])
        names(overlap) <- cnames[candidate]
      } else {
        rownames(step0) <- cnames
        step01 <- step0[candidate,]
        overlap <- apply(step01, 1, function(x){mean(x[!is.na(x)])})
      }
      
      select <- which(overlap == max(overlap))
      
      if (length(select)==1){
        final.select <- select
        final.select.idx <- which(names(select) == colnames(data.set))
        result <- c(result, names(final.select))
        result.idx <- c(result.idx, final.select.idx)
        rm.idx <- candidate[final.select]
      } else {
        minsel <- list()
        minnum <- vector()
        for (i in 1:length(select)){
          minsel[[i]] <- which(!is.na(counts[,candidate[select[i]]]))
          minnum[i] <- min(var.num[minsel[[i]]])
        }
        minselidx <- which(minnum == min(minnum))
        minlen <- length(minselidx)
        if (minlen == 1){
          final.select <- select[minselidx]
        } else {
          final.select <- sample(select[which(minnum == min(minnum))], 1)
        }
        final.select.idx <- which(names(final.select) == colnames(data.set))
        rm.idx <- candidate[final.select]
        result <- c(result, names(final.select))
        result.idx <- c(result.idx, final.select.idx)
      }
      
      coreset <- data.frame(data.set[, result.idx])
      colnames(coreset) <- result
      ide.num <- apply(coreset, 1, function(x){length(unique(x[!is.na(x)]))})
      coverage1 = round(mean(ide.num/var.num*100), 4)
      
      if (idx == 1 & prenum ==0){
        dy <- coverage1
      } else if (idx == 1 & prenum != 0){
        dy <- coverage1 - as.numeric(coverage.table[1,3])
      } else if ( idx > 1 & prenum != 0){
        dy <- coverage1 - as.numeric(coverage.table[idx, 3])
      } else {
        dy <- coverage1 - as.numeric(coverage.table[(idx-1), 3])
      }
      dy = round(dy, 4)
      coverage.table = rbind(coverage.table, c(idx, names(final.select), coverage1, dy))
      colnames(coverage.table) = c("Iteration","ID", "Coverage", "Difference")
      
      if(coverage1 >= coverage | dy < difference){
        break
      } else {
        step0 <- data.frame(step0[,-rm.idx])
        
        if (prenum == 0){
          for (i in 1:nc){
            idx1 <- which(as.vector(coreset[, idx]) == as.vector(counts[, i]))
            counts[idx1, i] = NA
          }
        } else {
          for (i in 1:nc){
            idx1 <- which(as.vector(coreset[, prenum + idx]) == as.vector(counts[,i]))
            counts[idx1,i] = NA
          }
        }
      }
    })
  }
  coverage.table <- as.data.frame(coverage.table)
  coreset <- as.data.frame(coreset)
  return(list(coverage.table = coverage.table, coreset = coreset))
}

IBS_analysis = function(data, Site_Info, REF, OBJ, Sliding.window = TRUE, window.size, step.size, remove_RM = TRUE){
  withProgress(message = "Processing Data", value = 0, {
    REF_loc = which(row.names(data) %in% REF)
    if (remove_RM) {
      RM = which(as.numeric(data[REF_loc,]) == 1 | is.na(data[REF_loc,]))
      if (length(RM) == 0) {
        data_HM = data
      } else {
        data_HM = data[,-RM]
        Site_Info = Site_Info[-RM,]
      }
    } else {
      data_HM = data
    }
    
    OBJ_loc = which(row.names(data_HM) %in% OBJ)
    Diff = as.numeric(data_HM[REF_loc,]) - as.numeric(data_HM[OBJ_loc,])
    
    nchr = length(unique(Site_Info$Chr))
    CHR = data.frame()
    start_pos = c()
    r = 1
    window_data = as.data.frame(0)
    
    if (Sliding.window == TRUE){
      Site_Info$Diff = Diff
      for (i in 1:nchr) {
        shiny::setProgress(value = i / nchr, message = sprintf("Processing Chromosome %d of %d", i, nchr))
        CHR = Site_Info[Site_Info$Chr == i,]
        for (n in 1:1000000) {
          start_pos[n] = step.size * (n-1)
          if (step.size * (n-1) >= max(CHR$Pos)){
            break
          }
        }
        for (j in 1:n) {
          loc = which(CHR$Pos >= start_pos[j] & 
                        CHR$Pos <= start_pos[j] + window.size)
          diff = which(CHR[loc, 4] != 0)
          if (length(loc) != 0){
            window_data[r, 1] = if_else(i < 10, paste0("Chr0", i), paste0("Chr", i))
            window_data[r, 2] = start_pos[j]
            window_data[r, 3] = start_pos[j] + window.size
            window_data[r, 4] = length(loc)
            window_data[r, 5] = length(diff)
            window_data[r, 6] = round((length(loc)-length(diff))/length(loc), 4) * 100
            r = r + 1
          }
        }
      }
      colnames(window_data) = c("Chr", "Start", "End", "Count", "Diff", "IBS_ratio")
      output = list(
        Num_Total_SNPs = dim(data_HM)[2],
        Num_Diff_SNPs = length(which(Diff != 0)),
        Num_Ide_SNPs = length(which(Diff == 0)),
        Num_NA_SNPs = length(which(is.na(Diff))),
        Diff_SNPs = colnames(data_HM)[which(Diff != 0)],
        window_data = window_data)
      return(output)
    } else {
      output = list(
        Num_Total_SNPs = dim(data_HM)[2],
        Num_Diff_SNPs = length(which(Diff != 0)),
        Num_Ide_SNPs = length(which(Diff == 0)),
        Num_NA_SNPs = length(which(is.na(Diff))),
        Diff_SNPs = colnames(data_HM)[which(Diff != 0)])
      return(output)
    }
  })
}