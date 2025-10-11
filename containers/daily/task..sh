#!/bin/bash

set -e  # exit immediately if any command fails

echo "[task.sh] [1/8] Starting Execution."
export TZ="HST"
echo "It is currently $(date)."
if [ $CUSTOM_DATE ]; then
    echo "An aggregation date was provided by the environment."
else
    export CUSTOM_DATE=$(date -d "1 day ago" --iso-8601)
    echo "No aggregation date was provided by the environment. Defaulting to yesterday."
fi
echo "Aggregation date is: " $CUSTOM_DATE

echo "[task.sh] [2/8] Pulling Mesonet data"
echo "--- begin AS_mesonet_yesterday_acquisition.R ---"
Rscript AS_mesonet_yesterday_acquisition.R $CUSTOM_DATE
echo "--- end AS_mesonet_yesterday_acquisition.R ---"

echo "[task.sh] [3/8] Pulling WRCC data"
echo "--- begin AS_WRCC_yesterday_acquisition.R ---"
Rscript AS_WRCC_yesterday_acquisition.R $CUSTOM_DATE
echo "--- end AS_WRCC_yesterday_acquisition.R ---"

echo "[task.sh] [4/8] Combining station data"
echo "--- begin as_nrt_combine.R ---"
Rscript as_nrt_combine.R $CUSTOM_DATE
echo "--- end as_nrt_combine.R ---"

echo "[task.sh] [5/8] Gapfilling station data"
echo "--- begin as_gapfill.R ---"
Rscript as_gapfill.R $CUSTOM_DATE
echo "--- end as_gapfill.R ---"

echo "[task.sh] [6/8] Running IDW interpolation and producing rainfall map"
echo "--- begin day_rf_IDW_derekversion_NRT.R ---"
Rscript day_rf_IDW_derekversion_NRT.R $CUSTOM_DATE
echo "--- end day_rf_IDW_derekversion_NRT.R ---"

echo "=== Pipeline complete! Outputs written to NRT subfolders ==="

echo "[task.sh] [7/8] Preparing upload config."
cd /sync
python3 inject_upload_config.py config.json $CUSTOM_DATE

echo "[task.sh] [8/8] Uploading data."
python3 upload.py

echo "[task.sh] All done!"

