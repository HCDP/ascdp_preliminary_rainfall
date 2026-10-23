#!/bin/bash

set -u

echo "[task.sh] [1/9] Starting Execution."
export TZ="Pacific/Pago_Pago"
echo "It is currently $(date)."
if [[ -v CUSTOM_DATE ]]; then
    echo "An aggregation date was provided by the environment."
else
    export CUSTOM_DATE=$(date -d "1 day ago" --iso-8601)
    echo "No aggregation date was provided by the environment. Defaulting to yesterday."
fi
echo "Aggregation date is: " $CUSTOM_DATE

source /workspace/envs/prod.env

cd /workspace/code


AGG_TS=$(date -d "$CUSTOM_DATE" +%s)
HISTORICAL_PIVOT_TS=$(date -d "2025-01-01" +%s)

if (( AGG_TS < HISTORICAL_PIVOT_TS )); then
    set -eo pipefail
    echo "Aggregation date falls before the historical pivot threshold. Downloading historical csv..."
    FNAME_FORMATTED_DATE=$(date -d "$CUSTOM_DATE" +%Y%m%d)
    WGET_OUTPUT_DIR="${OUTPUT_DIR%/}/as_gapfilled_data"
    mkdir -p "$WGET_OUTPUT_DIR"
    wget -c --tries=5 --waitretry=5 --timeout=15 -P "$WGET_OUTPUT_DIR" "https://api.hcdp.ikewai.org/files/download/ASCDP/workflow_data/preliminary/rainfall/dependencies/historical_data/as_gapfilled_data_historical/${FNAME_FORMATTED_DATE}_as_rf_idw_input_gapfilled.csv"
    echo "Download complete. Skipping steps 2-6..."
else
    # Data Acquisition
    echo "[task.sh] [2/9] Pulling Mesonet data"
    echo "--- begin AS_mesonet_yesterday_acquisition.R ---"
    Rscript AS_mesonet_yesterday_acquisition.R $CUSTOM_DATE
    echo "--- end AS_mesonet_yesterday_acquisition.R ---"

    echo "[task.sh] [3/9] Pulling WRCC data"
    echo "--- begin AS_WRCC_yesterday_acquisition.R ---"
    Rscript AS_WRCC_yesterday_acquisition.R $CUSTOM_DATE
    echo "--- end AS_WRCC_yesterday_acquisition.R ---"

    echo "[task.sh] [4/9] Pulling NWS data"
    echo "--- begin AS_NWS_yesterday_acquisition.R ---"
    Rscript AS_NWS_yesterday_acquisition.R $CUSTOM_DATE
    echo "--- end AS_NWS_yesterday_acquisition.R ---"

    set -eo pipefail

    echo "[task.sh] [5/9] Combining station data"
    echo "--- begin as_nrt_combine.R ---"
    Rscript as_nrt_combine.R $CUSTOM_DATE
    echo "--- end as_nrt_combine.R ---"

    echo "[task.sh] [6/9] Gapfilling station data"
    echo "--- begin as_gapfill.R ---"
    Rscript as_gapfill.R $CUSTOM_DATE
    echo "--- end as_gapfill.R ---"
fi



echo "[task.sh] [7/9] Running IDW interpolation and producing rainfall map"
echo "--- begin day_rf_IDW_derekversion_NRT.R ---"
Rscript day_rf_IDW_derekversion_NRT.R $CUSTOM_DATE
echo "--- end day_rf_IDW_derekversion_NRT.R ---"

echo "=== Pipeline complete! Outputs written to NRT subfolders ==="

echo "[task.sh] [8/9] Preparing upload config."
cd /sync
python3 inject_upload_config.py config.json $CUSTOM_DATE

echo "[task.sh] [9/9] Uploading data."
python3 upload.py

echo "[task.sh] All done!"

