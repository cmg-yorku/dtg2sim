@echo off

set modules=examples.Order examples.Build_2_1 examples.Build_2_2 examples.Build_2_3 examples.Build_2_4 examples.Heating_3_1 examples.Heating_3_2 examples.OrganizeTravel_4_1
set failed=0

for %%m in (%modules%) do (
    python -m %%m
    if errorlevel 1 (
        echo %%m: FAIL
        set /a failed+=1
    ) else (
        echo %%m: PASS
    )
)

echo.
echo Total failures: %failed%
exit /b %failed%


REM Run each test module as a Python module
REM python -m examples.Order
REM python -m examples.Build_2_1
REM python -m examples.Build_2_2
REM python -m examples.Build_2_3
REM python -m examples.Build_2_4
REM python -m examples.Heating_3_1
REM python -m examples.Heating_3_2
REM python -m examples.OrganizeTravel_4_1

echo Test batch done.