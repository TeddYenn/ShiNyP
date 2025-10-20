#' @title vcf2df
#' @export
vcf2df = function(VCF, diploidize) {
  withProgress(message = "Processing data...", value = 0, {
    if (diploidize == TRUE) {
      VCF_data = as.data.table(VCF[, 10:ncol(VCF)])
      total_steps = ncol(VCF_data) 
      for (i in seq_len(total_steps)) {
        shiny::incProgress(
          amount = 1 / total_steps,
          message = sprintf("Diploidizing data... (%d/%d)", i, total_steps)
        )
        VCF_data[[i]] = sapply(VCF_data[[i]], diploidize_genotype)
      }
    } else {
      VCF_data = VCF[, 10:ncol(VCF)]
      total_steps = ncol(VCF_data)
      for (i in 1:total_steps) {
        VCF_data[, i] = ifelse(VCF_data[, i] == "0/0", 0, 
                                ifelse(VCF_data[, i] %in% c("0/1", "1/0"), 1, 
                                       ifelse(VCF_data[, i] == "1/1", 2, NA)))
        incProgress(1 / total_steps, message = sprintf("Converting the data... (%d/%d)", i, total_steps))
      }
    }
    data = as.data.frame(t(VCF_data))
    colnames(data) = VCF$ID
    rownames(data) = colnames(VCF)[-c(1:9)]
    return(data)
  })
}

#' @title diploidize_genotype
#' @export
diploidize_genotype = function(gt_str) {
  if (is.na(gt_str) || gt_str %in% c("./.", ".|.")) {
    return(NA_real_)
  }
  alleles = unlist(strsplit(gt_str, "[/]")) 
  alleles = alleles[alleles != "."]
  if (length(alleles) == 0) {
    return(NA_real_)
  }
  unique_alleles = unique(alleles)
  alt_count = sum(unique_alleles != "0")
  fraction_alt = alt_count / length(unique_alleles)
  
  if (fraction_alt == 0) {
    return(0)
  } else if (fraction_alt == 1 && length(unique_alleles) == 1) {
    return(2)
  } else {
    return(1)
  }
}

#' @title vcf2Site_Info
#' @export
vcf2Site_Info = function(VCF) {
  Site_Info = VCF[, c(1:5)]
  colnames(Site_Info) = c("Chr", "Pos", "Marker", "Ref", "Alt")
  return(Site_Info)
}

#' @title hwe_test
#' @export
hwe_test = function(df) {
  N_AA = colSums(df == 0, na.rm = TRUE)
  N_Aa = colSums(df == 1, na.rm = TRUE)
  N_aa = colSums(df == 2, na.rm = TRUE)
  N = N_AA + N_Aa + N_aa
  p = (2 * N_AA + N_Aa) / (2 * N)
  q = 1 - p

  E_AA = p^2 * N
  E_Aa = 2 * p * q * N
  E_aa = q^2 * N

  chi_square = ((N_AA - E_AA)^2 / E_AA) + ((N_Aa - E_Aa)^2 / E_Aa) + ((N_aa - E_aa)^2 / E_aa)
  p_value = pchisq(chi_square, df = 1, lower.tail = FALSE)

  return(p_value)
}

#' @title Sampleplot
#' @export
Sampleplot = function(rate){
  par(mar = c(3, 4, 1, 1) + 0.1)
  cut = table(cut(rate, breaks = seq(0, 1.0, by = 0.1), include.lowest = T, labels = F))
  hist = hist(rate, freq = T, main = NULL, ylab = "Number of samples",
              xlab = "", ylim = c(0, max(cut)*1.1), col = "#e89978", labels = T)
  return(hist)
}

#' @title SNPplot
#' @export
SNPplot = function(rate){
  par(mar = c(3, 4, 1, 1) + 0.1)
  cut = table(cut(rate, breaks = seq(0, 1.0, by = 0.1), include.lowest = T, labels = F))
  hist = hist(rate, freq = T, main = NULL, ylab = "Number of SNPs",
              xlab = "", ylim = c(0, max(cut)), col = "#e89978", labels = T)
  return(hist)
}

#' @title SNPplot_HWE
#' @export
SNPplot_HWE = function(rate){
  par(mar = c(4, 4, 1, 1) + 0.1)
  hist = hist(rate, freq = T, main = NULL, ylab = "Number of SNPs",
              xlab = expression(-log[10](italic(p))), col = "#e89978", labels = T)
  return(hist)
}

#' @title stat2summary
#' @export
stat2summary = function(stat){
  summary = data.frame("Min" = min(stat, na.rm = T), "Max" = max(stat, na.rm = T), "Mean" = mean(stat, na.rm = T),
                       "Median" = median(stat, na.rm = T), "SD" = sd(stat, na.rm = T), "CV" = sd(stat, na.rm = T)/mean(stat, na.rm = T))
  summary[1:6] = format(round(summary[1:6], 4), nsmall = 4)
  return(summary)
}

#' @title stat2summary_HWE
#' @export
stat2summary_HWE = function(stat){
  summary = data.frame("Min" = min(stat, na.rm = T), "Max" = max(stat, na.rm = T), "Mean" = mean(stat, na.rm = T),
                       "Median" = median(stat, na.rm = T), "SD" = sd(stat, na.rm = T), "CV" = sd(stat, na.rm = T)/mean(stat, na.rm = T))
  summary[2:6] = format(round(summary[2:6], 4), nsmall = 4)
  summary[1] = format(summary[1], nsmall = 4)
  return(summary)
}

#' @title size2size
#' @export
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

#' @title popgen2
#' @export
popgen2 = function(M, subgroups = NULL){
  withProgress(message = 'Processing data...', value = 0, {
    incProgress(0.05, message = "Checking missing data...")
    hasAllMiss = colSums(is.na(M)) == nrow(M)
    if(any(hasAllMiss))
      warning("There are some markers with all data missing. These markers were removed from dataset")
    Z = as.matrix(M[, !hasAllMiss])

    incProgress(0.1, message = "Processing data...")
    if(is.null(subgroups))
      subgroups = 1
    labelSG = unique(subgroups)
    nSG = length(labelSG)

    incProgress(0.2, message = "Calculating statistics per sites...")
    general = g.of.p(Z)
    bygroup = c("There are no subgroups")

    if(nSG > 1){
      incProgress(0.3, message = "Calculating statistics by group...")
      bygroup = lapply(labelSG, function(i) g.of.p(Z[subgroups == i, ]))
      names(bygroup) = labelSG
      pbyg = sapply(X = as.vector(labelSG), FUN = function(x) bygroup[[x]]$Markers$p)

      for(i in 1:nSG){
        fxd = pbyg[,i] == 1 | pbyg[,i] == 0
        exc = (pbyg[,i]>0 & apply(pbyg[,-i, drop = FALSE] == 0, 1, all)) |
          (pbyg[,i]<1 & apply(pbyg[,-i, drop = FALSE] == 1, 1, all))
        if(sum(exc) == 0){
          excl = NA
        } else {
          excl = colnames(Z)[exc]
        }
        bygroup[[labelSG[i]]]$exclusive = excl
        if(sum(fxd) == 0){
          fix.g = NA
        } else {
          fix.g = colnames(Z)[fxd]
        }
        bygroup[[labelSG[i]]]$fixed = fix.g
      }
      incProgress(0.35, message = "Calculating F statistics...")
      ngroups = as.vector(table(subgroups))
      Hig = matrix(sapply(bygroup, function(x) x$Population["Ho","mean"]), ncol = nSG)
      His = sapply(bygroup, function(x) x$Markers[,"Ho"])
      Hss = sapply(bygroup, function(x) x$Markers[,"He"])
      Hsg = matrix(colMeans(Hss), ncol = nSG)
      Ht = matrix(general$Markers[,"He"], ncol = 1, dimnames = list(rownames(general$Markers), NULL))

      Fstatsg = F.stats(Hi = Hig, Hs = Hsg, Ht = mean(Ht), ngroups = ngroups)
      Fstatsm = F.stats(Hi = His, Hs = Hss, Ht = Ht, ngroups = ngroups)

      incProgress(0.4, message = "Calculating pairwise F statistics...")
      pw = combn(x = labelSG, m = 2)
      matFST = matrix(0, nrow = nSG, ncol = nSG, dimnames = list(labelSG, labelSG))

      Fstspw = round(data.frame("Fis" = numeric(ncol(pw)+1),
                                "Fst" = numeric(ncol(pw)+1),
                                "Fit" = numeric(ncol(pw)+1),
                                row.names = c("All_Pop", paste(pw[1,], pw[2,], sep = "-"))), 4)
      Fstspw[1,] = Fstatsg

      for(i in 1:ncol(pw)){
        sel = labelSG %in% pw[,i]
        nsbg = ngroups[sel]
        Hisg = Hig[,sel, drop=FALSE]
        Hssg = Hsg[,sel, drop=FALSE]
        Fstspw[i+1,] = F.stats(Hi = Hisg, Hs = Hssg, Ht = mean(Ht), ngroups = nsbg)
        matFST[pw[1,i], pw[2,i]] = matFST[pw[2,i], pw[1,i]] = Fstspw[i+1, 2]
      }

      Fstats = list("Genotypes" = Fstspw, "Markers" = Fstatsm)

      bygroup = c(bygroup, list("F.stats" = Fstats))
    }
    incProgress(0.5, message = "Finished processing data...")

    out = list("whole" = general, "bygroup" = bygroup)
    return(out)
  })
}

#' @title g.of.p
#' @export
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

#' @title F.stats
#' @export
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

#' @title inbreeding.fun
#' @export
inbreeding.fun = function(mat, p){
  nOHom = rowSums(mat != 1, na.rm = T)
  nEHom = 1 - (2* p *(1-p))
  EH = as.vector(round(nEHom %*%  t(!is.na(mat))))
  Fi = round((nOHom - EH)/(rowSums(!is.na(mat)) - EH), 4)
  return(Fi)
}

#' @title chiS
#' @export
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

#' @title GDsiteplot
#' @export
GDsiteplot = function(data, y_axis, y_label){
  for (i in seq_along(unique(data$Chr))) {
    if (i > 1) {
      end = max(data[data$Chr == (i - 1), "Pos"])
      data[data$Chr == i, "Pos"] = as.numeric(data[data$Chr == i, "Pos"]) + end
    }
  }
  data$Pos = as.numeric(data$Pos)

  Chr_axis = data %>%
    group_by(Chr) %>%
    summarise(center = (max(Pos) + min(Pos)) / 2, .groups = "drop")

  data$Chr = factor(data$Chr, levels = sort(unique(as.numeric(as.character(data$Chr)))))
  n_chr = length(levels(data$Chr))
  colors = rep(c("#A4BE7B", "#dfaa52"), length.out = n_chr)

  plot = ggplot(data, aes(x = Pos, y = get(y_axis))) +
    geom_point(aes(color = as.factor(Chr)), alpha = 0.6, size = 0.8) +
    scale_color_manual(values = colors) +
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

#' @title GDgroupplot
#' @export
GDgroupplot = function(data, y_axis, y_label){
  data = data[-1,]
  data2 = data.frame(
    x = row.names(data),
    y = data[, which(colnames(data) == y_axis)]
  )
  colors = colorRampPalette(custom_palette)(length(unique(data2$x)))
  plot = ggplot(data2, aes(x = x, y = y, color = x)) +
    geom_segment(aes(x = x, xend = x, y = 0, yend = y), color = "grey50") +
    geom_point(size = 7) +
    theme_classic() +
    xlab("") +
    ylab(y_label) +
    scale_color_manual(values = colors) +
    scale_y_continuous(expand = c(0.025, 0)) +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 13),
      axis.title.y = element_text(size = 16)
    )
  return(plot)
}

#' @title chromosome
#' @export
chromosome = function(chr) {
  num = as.numeric(gsub("\\D", "", chr))
  if (num < 10) {
    return(paste0("Chr0", num))
  } else {
    return(paste0("Chr", num))
  }
}

#' @title generateCircosPlot
#' @export
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
                  col = "grey", labels.col = "grey50", lwd = 0.7, labels.facing = "clockwise")}, bg.border = NA)
    shiny::setProgress(value = 2 / track, message = sprintf("Processing Track %d of %d", 2, track))
    circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
      circos.text(CELL_META$xcenter, CELL_META$ycenter, CELL_META$sector.index,
                  facing = "inside", niceFacing = T, col = "grey30", cex = 0.9)
    }, bg.col = rep(c("#EEE3CB", "#D7C0AE"), 20), bg.border = NA, track.height = 0.05)
    # Track 3
    if (!is.null(Track3)){
      shiny::setProgress(value = 3 / track, message = sprintf("Processing Track %d of %d", 3, track))
      if (Track3 == "SNP Density"){
        Track3 = SW_data[, c(1:4)]
        circos.genomicTrack(Track3, panel.fun = function(region, value, ...) {
          circos.genomicRect(region, value,
                             col = colorRamp2(c(0, median(Track3[,4]), max(Track3[,4])), c("#f3f4e4", "#c5c678", "#a9aa48"))(value),
                             border = NA)
        }, bg.border = NA, track.height = 0.1, track.margin = c(0.001, 0.001))
      } else{
        Track3 = SW_data[, c(1:3, which(colnames(SW_data) == Track3))]
        top = quantile(Track3[,4], probs = 0.99)
        circos.genomicTrack(Track3, panel.fun = function(region, value, ...) {
          circos.genomicLines(region, value, type = "h", lwd = ifelse(value[[1]]>=top, 0.75, 0.55), col = ifelse(value[[1]]>=top, "#ff7878", "#A4BE7B"))
        }, bg.border = NA, ylim = c(0, max(Track3[,4])+0.001), track.height = 0.13)
      }
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
        circos.genomicLines(region, value, type = "h", lwd = ifelse(value[[1]]>=top, 0.65, 0.45), col = ifelse(value[[1]]>=top, "#ff7878", "#e3c47c"))
      }, bg.border = NA, ylim = c(0, max(Track5[,4])+0.001), track.height = 0.13)
    }
    # Track 6
    if (!is.null(Track6)){
      shiny::setProgress(value = 6 / track, message = sprintf("Processing Track %d of %d", 6, track))
      Track6 = SW_data[,c(1:3, which(colnames(SW_data) == Track6))]
      top = quantile(Track6[,4], probs = 0.99)
      circos.genomicTrack(Track6, panel.fun = function(region, value, ...) {
        circos.genomicLines(region, value, type = "h", lwd = ifelse(value[[1]]>=top, 0.55, 0.4), col = ifelse(value[[1]]>=top, "#ff7878", "#b695cc"))
      }, bg.border = NA, ylim = c(0, max(Track6[,4])+0.001), track.height = 0.13)
    }
  })
  dev.off()
}

#' @title core.set
#' @export
core.set = function(data.set, coverage = coverage, difference = difference){
  counts = data.set
  nc = ncol(data.set)
  nr = nrow(data.set)
  difference = difference
  coverage1 = NULL
  coverage.table = NULL
  rnames = rownames(data.set)
  cnames = colnames(data.set)

  var.num = vector()
  ide.num = rep(0, nr)
  var.num = apply(data.set, 1, function(x){length(unique(x[!is.na(x)]))})
  mpe=sum(var.num - 1)

  result = NULL
  result.idx = NULL
  overlap.score = function(x){
    tmp1 = x
    tmp1.table = table(tmp1)
    idx = match(tmp1, names(tmp1.table))
    dummy = tmp1.table[idx]
    na.idx = which(is.na(idx))
    if (length(na.idx) > 0){
      dummy[na.idx] = 0
    }
    dummy
  }
  prenum = 0
  step0 = NULL
  step0 = data.frame(apply(counts, 1, overlap.score))
  rownames(step0) = cnames

  for (idx in 1:mpe){
    withProgress(message = paste("Running: iteration ", idx, "..."), value = NULL, {
      not.na.counts = apply(counts, 2, function(x){
        length(which(!is.na(x)))
      })
      rnames = rownames(counts)
      cnames = colnames(counts)
      candidate = which(not.na.counts == max(not.na.counts))
      if (length(candidate) == 1){
        step01 = step0[candidate,]
        overlap = mean(step01[!is.na(step01)])
        names(overlap) = cnames[candidate]
      } else {
        rownames(step0) = cnames
        step01 = step0[candidate,]
        overlap = apply(step01, 1, function(x){mean(x[!is.na(x)])})
      }

      select = which(overlap == max(overlap))

      if (length(select)==1){
        final.select = select
        final.select.idx = which(names(select) == colnames(data.set))
        result = c(result, names(final.select))
        result.idx = c(result.idx, final.select.idx)
        rm.idx = candidate[final.select]
      } else {
        minsel = list()
        minnum = vector()
        for (i in 1:length(select)){
          minsel[[i]] = which(!is.na(counts[,candidate[select[i]]]))
          minnum[i] = min(var.num[minsel[[i]]])
        }
        minselidx = which(minnum == min(minnum))
        minlen = length(minselidx)
        if (minlen == 1){
          final.select = select[minselidx]
        } else {
          final.select = sample(select[which(minnum == min(minnum))], 1)
        }
        final.select.idx = which(names(final.select) == colnames(data.set))
        rm.idx = candidate[final.select]
        result = c(result, names(final.select))
        result.idx = c(result.idx, final.select.idx)
      }

      coreset = data.frame(data.set[, result.idx])
      colnames(coreset) = result
      ide.num = apply(coreset, 1, function(x){length(unique(x[!is.na(x)]))})
      coverage1 = round(mean(ide.num/var.num*100), 4)

      if (idx == 1 & prenum ==0){
        dy = coverage1
      } else if (idx == 1 & prenum != 0){
        dy = coverage1 - as.numeric(coverage.table[1,3])
      } else if ( idx > 1 & prenum != 0){
        dy = coverage1 - as.numeric(coverage.table[idx, 3])
      } else {
        dy = coverage1 - as.numeric(coverage.table[(idx-1), 3])
      }
      dy = round(dy, 4)
      coverage.table = rbind(coverage.table, c(idx, names(final.select), coverage1, dy))
      colnames(coverage.table) = c("Iteration","ID", "Coverage", "Difference")

      if(coverage1 >= coverage | dy < difference){
        break
      } else {
        step0 = data.frame(step0[,-rm.idx])

        if (prenum == 0){
          for (i in 1:nc){
            idx1 = which(as.vector(coreset[, idx]) == as.vector(counts[, i]))
            counts[idx1, i] = NA
          }
        } else {
          for (i in 1:nc){
            idx1 = which(as.vector(coreset[, prenum + idx]) == as.vector(counts[,i]))
            counts[idx1,i] = NA
          }
        }
      }
    })
  }
  coverage.table = as.data.frame(coverage.table)
  coreset = as.data.frame(coreset)
  return(list(coverage.table = coverage.table, coreset = coreset))
}

#' @title IBS_analysis
#' @export
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

#' @title density_analysis
#' @export
density_analysis = function(Site_Info, Chr_Info, window.size){
  withProgress(message = "Processing Data", value = 0, {
    nchr = length(unique(Site_Info$Chr))
    CHR = data.frame()
    start_pos = numeric()
    r = 1
    window_data = data.frame(matrix(ncol = 4, nrow = 0))

    for (i in 1:nchr) {
      shiny::setProgress(value = i / nchr, message = sprintf("Processing Chromosome %d of %d", i, nchr))

      CHR = Site_Info[Site_Info$Chr == i, ]
      max = Chr_Info$Length[i]
      n = 1
      repeat {
        if (n == 1) {
          start_pos[n] = 0
        } else {
          start_pos[n] = window.size * (n - 1)
        }

        if (start_pos[n] + window.size >= max) {
          break
        }
        n = n + 1
      }

      for (j in 1:n) {
        loc = which(CHR$Pos >= start_pos[j] & CHR$Pos <= start_pos[j] + window.size)
        if (length(loc) > 0) {
          window_data[r, 1] = if_else(i < 10, paste0("Chr0", i), paste0("Chr", i))
          window_data[r, 2] = start_pos[j]
          window_data[r, 3] = start_pos[j] + window.size
          window_data[r, 4] = length(loc)
          r = r + 1
        }
      }
    }

    colnames(window_data) = c("Chr", "Start", "End", "Count")
    return(window_data)
  })
}



#' @title my_palette
#' @export
my_palette = function(scatter_color, n_groups) {
  custom_palette = switch(scatter_color,
                          "Default" = custom_palette,
                          "Black - single color" = c("black"),
                          "Grey - single color" = c("grey"),
                          "Bright" = c("#90AACB", "#88D66C", "#FDDE55", "#ffb366", "#FFA38F", "#bb738b"),
                          "Vivid" = c("#E41A1C", "#FF7F00", "#FFFF33", "#4DAF4A", "#377EB8", "#984EA3"),
                          "Viridis" = c("#fdd525", "#7AD151FF", "#22A884FF", "#2A788EFF", "#414487FF", "#440154FF"),
                          "Metro" = c("#AC3EC1FF", "#477BD1FF", "#46B298FF", "#90BA4CFF", "#DD9D31FF", "#E25247FF"),
                          "Vibrant" = c("#ff8700", "#ffd300", "#deff0a", "#a1ff0a", "#0aff99", "#0aefff", "#147df5", "#580aff", "#be0aff", "#ff1a1a"),
                          "Red" = "red",
                          "Dark red" = "darkred",
                          "Black" = "black",
                          "Grey" = "grey"
  )
  if (length(custom_palette) == 1) {
    colors = rep(custom_palette, n_groups)
  } else if (length(custom_palette) >= n_groups) {
    colors = custom_palette[1:n_groups]
  } else {
    colors = colorRampPalette(custom_palette)(n_groups)
  }
  return(colors)
}

#' @title parse_sections
#' @export
parse_sections = function(text) {
  lines = unlist(strsplit(text, "\n"))
  
  sections = list()
  current_section = NULL
  current_subsection = NULL
  current_content = c()
  
  for (line in lines) {
    if (grepl("^## [^#]", line)) {
      if (!is.null(current_section)) {
        sections[[current_section]][[current_subsection]] = paste(current_content, collapse = "\n")
      }
      current_section = trimws(sub("^## ", "", line))
      sections[[current_section]] = list()
      current_subsection = "Overview"
      current_content = c()
    } else if (grepl("^### ", line)) {
      if (!is.null(current_subsection)) {
        sections[[current_section]][[current_subsection]] = paste(current_content, collapse = "\n")
      }
      current_subsection = trimws(sub("^### ", "", line))
      current_content = c()
    } else {
      current_content = c(current_content, line)
    }
  }
  
  if (!is.null(current_section) && !is.null(current_subsection)) {
    sections[[current_section]][[current_subsection]] = paste(current_content, collapse = "\n")
  }
  
  return(sections)
}

#' @title llama_retrieve_answer
#' @export
llama_retrieve_answer <- function(query_text) {
  api_key <- "llx-UwNIf51ZaC1v0F9pmZH1MWeWUbfWE2o2waprBQ70Y4AQ1F48"
  endpoint_url <- "https://api.cloud.llamaindex.ai/api/v1/pipelines/2558b3ba-5702-46a2-9598-bdd273e46f9d/retrieve"
  
  response <- POST(
    url = endpoint_url,
    add_headers(
      Authorization = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ),
    body = list(query = query_text),
    encode = "json"
  )
  
  if (status_code(response) == 200) {
    result <- content(response, as = "parsed", encoding = "UTF-8")
    answer <- paste(
      vapply(result$retrieval_nodes[1:4], function(x) x$node$text, character(1)),
      collapse = "\n\n"
    )
    return(answer)
  } else {
    return(status_code(response))
  }
}
