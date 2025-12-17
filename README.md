## Important Note of Model Computational Efficiency

This analysis involves fitting **485,577 linear models** (one per CpG site), which is computationally intensive.

**Caching system implemented:**

- **First run**: Takes ~10-20 minutes total
  - Download dataset: ~2-5 min (500MB file)
  - Fit linear models: ~7-15 min
- **Subsequent runs**: Takes ~1-2 minutes (loads cached results)
- **Cache location**: `data/` folder (created automatically)
- **Cache files**:
  - `GSE66351.rds` - Downloaded dataset (avoids re-downloading)
  - `cpg_results.rds` - Linear model results (avoids refitting)

**If download fails:**

If you get timeout errors, the dataset file is too large for your connection. Try:

1. **Increase timeout**
2. **Try on a different network** (university/work connections are faster)
3. **Manual download alternative:**
   - Visit: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE66351
   - Click "Series Matrix File(s)" → Download `GSE66351_series_matrix.txt.gz`
   - The code will attempt automatic retry 3 times before failing

**To force re-analysis:** Delete the `data/` folder and re-knit.

This document can be knitted intirely without any manual intervention. Just click "Knit" and wait ~15-20 minutes on first run.
