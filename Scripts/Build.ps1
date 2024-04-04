param([string[]]$Languages = $("csharp", "python", "cpp", "go"))

$Success = $true

if ("csharp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C# ==="
    Push-Location ./csharp
    dotnet build ./Turing -c:Release
    $Success = $Success -and $?
    dotnet build ./Turing.Tests -c:Release
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("python" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Python ==="
    Push-Location ./python
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("cpp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C++ ==="
    Push-Location ./cpp
    cmake .
    $Success = $Success -and $?
    cmake --build .
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("go" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Go ==="
    Push-Location ./go
    go build
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ($Success) {
    Write-Host "`nBuild successful!" -ForegroundColor:Green
    exit 0
}
else {
    Write-Host "`nBuild failed" -ForegroundColor:Red
    exit 1
}