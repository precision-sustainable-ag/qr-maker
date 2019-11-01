library(grid)
library(gridExtra)
loadNamespace("qrencoder")
# library(qrencoder)
# qrencoder must be installed and loaded 
#   but NOT attached via `library` or `require`


# grid and gridExtra are used for drawing and arranging graphics objects.
# qrencoder renders the QR codes as matrices.


# Add a white margin to the QR code
marginizer <- function(mat, n = 1) {
  tb <- matrix(0, nrow = n,               ncol = ncol(mat))
  lr <- matrix(0, nrow = nrow(mat) + 2*n, ncol = n        )
  
  cbind(lr, rbind(tb, mat, tb), lr)
}




# Newest version without 1D barcode
weed_draw <- function(lbl, level = 2) {
  gp = gpar(fontsize = 75, fontfamily = "mono", fontface = "bold", lineheight = 0.8)
  
  # `1 - QR` to invert 1/0=black/white; `t` to orient correctly
  qr = t(qrencoder::qrencode_raw(lbl, level = level))
  grob_qr = rasterGrob(1 - marginizer(qr, n = 2), interpolate = F)
  
  grob_human = textGrob(paste0("\n", lbl, "\ny____m__d__\n"), gp = gp)
  
  grid.arrange(
    grob_human, grob_qr, 
    heights = c(0.35,0.65)
  )
}



# This function takes a vector of strings, opens a PDF file, loops over the
# vector, and writes each to the PDF file with a margin.
weed_sheets <- function(lbls, file = NULL) {
  
  if (is.null(file)) {stop("Please supply a filename.")}
  
  if (!is.character(lbls)) {stop("Please supply a vector of labels as the first argument.")}
  
  pdf(file, width = 8, height = 11, paper = "US", 
      family = "mono", pagecentre = T, title = basename(file))
  lapply(lbls, weed_draw)
  dev.off()
  invisible()
}


#### TO USE: ----
# Preview:
#weed_draw("W ABC B0 T1")


# To pass in a vector of pregenerated barcodes:
#weed_sheets(c("W ABC B0 T1", "W ABC B1 T2", "W ABC B3 T3", "W ABC C4 T1"), "test.pdf")



# What I think the proposed structure of the barcodes is:
# test_weeds_df <- expand.grid(
#   siteprefix = "W XYZ ", 
#   trt = c("B", "C"), 
#   quadrat = 0:9, 
#   timing = paste0(" T", 1:3)
#   )
# 
# test_weeds <- apply(test_weeds_df, 1, paste, collapse = "")
# test_weeds
# 
# weed_sheets(test_weeds, "XYZ.pdf")




# replace with one of your generated lists of barcodes to test
# test <- readr::read_lines("C:/Users/Brian.Davis/Downloads/NC_barcodes_for_monday.csv")
# weed_sheets(test, "longtest.pdf")


