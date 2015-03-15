@echo off
PATH %~dp0\bin;%PATH%
set TMPDIR=%TMP%
echo Put GCC in PATH, perhaps git (thus bash flex etc.) then gmake