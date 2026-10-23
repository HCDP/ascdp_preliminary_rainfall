#!/bin/bash

set -u

echo "[task.sh] [1/4] Starting Execution."
export TZ="Pacific/Pago_Pago"
echo "It is currently $(date)."
if [[ -v CUSTOM_DATE ]]; then
    echo "An aggregation date was provided by the environment."
else
    export CUSTOM_DATE=$(date -d "last month" +%Y-%m-01)
    echo "No aggregation date was provided by the environment. Defaulting to last month."
fi
echo "Aggregation date is: " $CUSTOM_DATE

source /workspace/envs/prod.env

cd /workspace/code

# Monthly IDW Interpolation
echo "[task.sh] [2/4] Running IDW interpolation and producing monthly rainfall map"
echo "--- begin as_rf_IDW_month.R ---"
Rscript as_rf_IDW_month.R $CUSTOM_DATE
echo "--- end as_rf_IDW_month.R ---"

echo "=== Pipeline complete! Outputs written to NRT subfolders ==="

echo "[task.sh] [3/4] Preparing upload config."
cd /sync
python3 inject_upload_config.py config.json $CUSTOM_DATE

echo "[task.sh] [4/4] Uploading data."
python3 upload.py

echo "[task.sh] All done!"