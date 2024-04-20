param([string[]]$Languages = $("csharp","python","cpp","go","pascal","ada"))

$Success = $true

if ("csharp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C# ==="
    Push-Location ./csharp
    dotnet run --project ./Turing -c:Release --no-build toml "../Samples/Substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("python" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Python ==="
    Push-Location ./python
    python3 ./turing.py toml "../Samples/Substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("cpp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C++ ==="
    Push-Location ./cpp
    ./turing toml "../Samples/Substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("go" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Go ==="
    Push-Location ./go
    ./turing toml "../Samples/Substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("pascal" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Pascal ==="
    Push-Location ./pascal
    ./Turing toml "../Samples/Substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("ada" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Ada ==="
    Push-Location ./ada
    ./bin/turing toml "../Samples/Substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ($Success) {
    Write-Host "`nRun successful!" -ForegroundColor:Green
    exit 0
}
else {
    Write-Host "`nRun failed" -ForegroundColor:Red
    exit 1
}
