@echo off
setlocal

REM ============================================================
REM ANA-SNIRH Spatial Station Finder
REM Actualiza GitHub con todos los cambios del proyecto.
REM Si no hay cambios, simplemente hace push por si hubiera
REM commits locales pendientes.
REM ============================================================

cd /d "E:\BASE DE DATOS\ANA_SNIRH_WEB"

echo.
echo ============================================
echo ANA-SNIRH - ACTUALIZAR GITHUB
echo ============================================
echo.

REM Verificar que Git este disponible
git --version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Git no esta disponible en PATH.
    pause
    exit /b 1
)

REM Agregar todos los archivos nuevos/modificados/eliminados
echo [1/4] Revisando cambios...
git add -A
if errorlevel 1 (
    echo ERROR durante git add.
    pause
    exit /b 1
)

REM Mostrar estado
git status --short

REM Comprobar si hay cambios staged
git diff --cached --quiet
if errorlevel 1 (
    echo.
    echo [2/4] Creando commit...
    git commit -m "Update ANA-SNIRH Spatial Station Finder"
    if errorlevel 1 (
        echo ERROR durante git commit.
        pause
        exit /b 1
    )
) else (
    echo.
    echo [2/4] No hay cambios nuevos para commit.
)

REM Push siempre, por si existe algun commit local pendiente
echo.
echo [3/4] Enviando a GitHub...
git push
if errorlevel 1 (
    echo ERROR durante git push.
    pause
    exit /b 1
)

echo.
echo [4/4] Listo.
echo Repositorio actualizado correctamente.
echo.
pause

endlocal
