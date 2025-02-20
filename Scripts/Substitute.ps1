param([string[]]$Languages = $("csharp", "python", "cpp", "go", "pascal", "ada", "c", "d", "rust"))

$Success = $true

if ("csharp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C# ==="
    Push-Location ./csharp
    dotnet run --project ./Turing -c:Release --no-build toml "../Samples/substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("python" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Python ==="
    Push-Location ./python
    python3 ./turing.py toml "../Samples/substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("cpp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C++ ==="
    Push-Location ./cpp
    ./turing toml "../Samples/substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("go" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Go ==="
    Push-Location ./go
    ./turing toml "../Samples/substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("pascal" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Pascal ==="
    Push-Location ./pascal
    ./Turing toml "../Samples/substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("ada" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Ada ==="
    Push-Location ./ada
    ./bin/turing toml "../Samples/substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("c" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C ==="
    Push-Location ./c
    ./turing toml "../Samples/substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("d" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== D ==="
    Push-Location ./d
    ./turing -m toml -f ../Samples/substitute.tom -i abba
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("rust" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Rust ==="
    Push-Location ./rust
    cargo run -- toml ../Samples/substitute.tom abba
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
