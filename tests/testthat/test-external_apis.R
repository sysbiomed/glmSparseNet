context("external_apis")

test_that("gene name query", {
  target <- data.frame(ensembl = c('ENSG00000114978',
                                   'ENSG00000166211',
                                   'ENSG00000183688',
                                   'ENSG00000171862',
                                   'ENSG00000141510'),
                       gene.name = c('MOB1A',
                                    'SPIC',
                                    'RFLNB',
                                    'PTEN',
                                    'TP53'),
                       stringsAsFactors = FALSE,
                       row.names = 1)

  res <- gene.names(rownames(target))
  expect_equal(res$external_gene_name, target[res$ensembl_gene_id, 'gene.name'])
})

test_that("protein query", {
  target <- data.frame(ensembl = c('ENSP00000379364',
                                   'ENSP00000299272',
                                   'ENSP00000331915',
                                   'ENSP00000361021',
                                   'ENSP00000269305'),
                       gene.name = c('ENSG00000114978',
                                     'ENSG00000166211',
                                     'ENSG00000183688',
                                     'ENSG00000171862',
                                     'ENSG00000141510'),
                       stringsAsFactors = FALSE,
                       row.names = 1)

  res <- protein.to.ensembl.gene.names(rownames(target))
  expect_equal(res$ensembl_gene_id, target[res$ensembl_peptide_id, 'gene.name'])
})

