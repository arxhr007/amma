# Regression runner for the interpreter fixes.
# Requires a working linker (VS Build Tools "Desktop development with C++",
# or a GNU rust target + MinGW). Build, then run each case and compare output.
#
#   powershell -ExecutionPolicy Bypass -File samples\regression\run_regression.ps1

$ErrorActionPreference = "Stop"
cargo build --release
$amma = ".\target\release\amma.exe"

function Check($file, $expected) {
    $got = (& $amma $file 2>&1 | Out-String).Trim() -replace "`r",""
    $exp = $expected.Trim() -replace "`r",""
    if ($got -eq $exp) {
        Write-Host "PASS  $file" -ForegroundColor Green
    } else {
        Write-Host "FAIL  $file" -ForegroundColor Red
        Write-Host "  expected: $($exp -replace "`n","\n")"
        Write-Host "  got:      $($got -replace "`n","\n")"
    }
}

Check "samples\regression\b2_early_return.amma"     "1"
Check "samples\regression\b1_nested_loops.amma"     "0`n0`n1`n0`n1`n0`n1`n1"
Check "samples\regression\b4_unary_minus.amma"      "-5`n-3"
Check "samples\regression\b5_forward_ref.amma"      "42"
Check "samples\regression\b3_expr_loop_start.amma"  "2`n3`n4"
Check "samples\regression\e1_bad_index.amma"        'Runtime error: Index must be an integer, got "z"'
