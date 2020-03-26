#' random forest feature selection based on binomial exact test
#'
#' \code{binomialRF} is the R implementation of the feature selection algorithm by (Zaim 2019)
#' 
#'                       
#' @param binomialRF_object the binomialRF object output 
#' @param gene_ontology a two- or three-column representation of a gene ontology with gene and geneset names
#' @param cutoff a real-valued number between 0 and 1, used as a p-value threshold

#' @references Zaim, SZ; Kenost, C.; Lussier, YA; Zhang, HH. binomialRF: Scalable Feature Selection and Screening for Random Forests to Identify Biomarkers and Their Interactions, bioRxiv, 2019.
#'
#' @return a data.frame with 4 columns: Geneset Name, P-value, Adjusted P-value based on \code{fdr.method}
#'
#'
#' @export


geneset_binomialRF <- function(binomialRF_object, gene_ontology, cutoff=0.2){
  
  if(!is.numeric(cutoff) ){
    stop("Error: the cutoff parameter should be a numeric input")
  } 
  
  binomialRF_object <- binomialRF_object[binomialRF_object$adjSignificance < cutoff,]
  
  enrich_pathway <- function(pathway, gene_ontology, binomialRF_object){
    pathway_genes = gene_ontology$symbol[gene_ontology$goid ==pathway]
    
    a = sum(binomialRF_object$variable %in% pathway_genes)
    b = length(pathway_genes)
    c = sum(!binomialRF_object$variable %in% pathway_genes)
    d = length(unique(gene_ontology$symbol)) -b
    
    mat = matrix(c(a,b,c,d), ncol=2, byrow = T)
    return(c(pathway,stats::fisher.test(mat, alternative = 'greater')$p.value))  
  }
  
  pathway_scores = lapply(unique(gene_ontology$goid), function(x) enrich_pathway(x, gene_ontology, binomialRF_object))
  pathway_scores = data.frame(do.call(rbind, pathway_scores),stringsAsFactors = F)
  colnames(pathway_scores) <- c('Geneset','P_value')
  pathway_scores$P_value <- as.numeric(pathway_scores$P_value)
  pathway_scores$AdjPvalue <- stats::p.adjust(pathway_scores$P_value, method = 'BY' )
  pathway_scores <- pathway_scores[order(pathway_scores$AdjPvalue, decreasing = F),]

  return(pathway_scores)
  
}