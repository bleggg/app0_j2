df <- 
  aws.s3::s3read_using(
    FUN = readr::read_csv2,
    object = "diffusion/bonnes-pratiques-r/rp_2016_individu_sample.csv",
    bucket = "projet-formation",
    opts = list("region" = "")
  )

readr::write_csv2(df, "data/individu_reg.csv")
arrow::write_parquet(df, "data/individu_reg.parquet")
