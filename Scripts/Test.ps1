param([string[]]$Languages = $("csharp", "python","cpp","go","pascal", "ada", "c"))

$Success = $true

if ("csharp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C# ==="
    Push-Location ./csharp
    dotnet test ./Turing.Tests -c:Release
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("python" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Python ==="
    Push-Location ./python
    python3 test.py
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("cpp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C++ ==="
    Push-Location ./cpp
    ./turing-tests
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("go" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Go ==="
    Push-Location ./go
    go test ./interpreter/
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("pascal" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Pascal ==="
    Push-Location ./pascal
    ./TuringTest
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("ada" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "`n=== Ada ==="
    Push-Location ./ada/test
    alr run
    $Success = $Success -and $?
    Pop-Location
}

if ("c" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C ==="
    Push-Location ./c
    ./turing-tests
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ($Success) {
    Write-Host "`nTest successful!" -ForegroundColor:Green
    exit 0
}
else {
    Write-Host "`nTest failed" -ForegroundColor:Red
    exit 1
}
