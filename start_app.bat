@echo off
rem Starts the GLD_FitExplore Shiny server on port 4322 (if not already running),
rem waits until it answers, then opens gld_app.html (which embeds the app) in the default browser.
set RSCRIPT="C:\Program Files\R\R-4.6.0\bin\Rscript.exe"
set APPDIR=%~dp0

netstat -ano | findstr /R /C:":4322 .*LISTENING" >nul
if %errorlevel%==0 (
  echo Server already running on port 4322.
  goto open
)
echo Starting the GLD app server on port 4322 ...
start "GLD app server" /min %RSCRIPT% -e "shiny::runApp('%APPDIR:\=/%', port = 4322, launch.browser = FALSE)"
set /a n=0
:wait
timeout /t 2 /nobreak >nul
netstat -ano | findstr /R /C:":4322 .*LISTENING" >nul
if %errorlevel%==0 goto open
set /a n+=1
if %n% lss 45 goto wait
echo The server did not start within 90 seconds. Check the "GLD app server" window for errors.
pause
exit /b 1

:open
echo Server is up. Opening the app ...
rem ?local=1 makes gld_app.html show the local server instead of the public Connect Cloud copy
start "" "file:///%APPDIR:\=/%gld_app.html?local=1"
