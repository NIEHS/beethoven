-- !preview conn=DBI::dbConnect(duckdb::duckdb(), read_only = TRUE)

-- This query reads K parquet files using a wildcard or comma-separated list.
-- `params.parquet_paths` should be a string like:
--   "'file1.parquet', 'file2.parquet'"  -- (multiple explicit files)
--   OR
--   "'path/to/files/prefix*.parquet'"  -- (wildcard match)

with combined as (
  select * from read_parquet({{ params.parquet_paths }})
)

select *
from combined;
