USE AdventureWorks2019;
GO

SELECT JobTitle
      ,BirthDate
      ,MaritalStatus
      ,Gender
      ,HireDate
      ,SalariedFlag
      ,VacationHours
      ,SickLeaveHours
      ,CurrentFlag
FROM HumanResources.Employee;

-- Características importantes de un empleado.
SELECT *
FROM HumanResources.v_Employee;

SELECT TOP 100 *
FROM HumanResources.v_EmployeeSales;

-- Frecuencia absoluta de Género
SELECT Gender
       ,COUNT(*) AS Frequency
FROM HumanResources.v_Employee
GROUP BY Gender
ORDER BY Frequency DESC;

SELECT Gender
      ,AgeBucket
FROM HumanResources.v_Employee;

-- Frecuencia absoluta de Edad (observaciones mayores a 30)
SELECT Age
       ,COUNT(*) AS Frequency
FROM HumanResources.v_Employee
GROUP BY Age
ORDER BY Frequency DESC;

-- Cálculo de distribución de frecuencia agrupada para Edad
DECLARE @Rango AS SMALLINT
       ,@IntervaloVelleman AS NUMERIC(4,1)
       ,@IntervaloSturges AS NUMERIC(4,1);

-- 1. Calcular el Rango.
SELECT @Rango = MAX(Age) - MIN(Age)
FROM HumanResources.v_Employee

SELECT @Rango; -- Rango es 40

-- Determinar el num de intervalos
-- Velleman
SELECT @IntervaloVelleman = SQRT(COUNT(*))
FROM HumanResources.v_Employee;

SELECT @IntervaloVelleman; -- El intervalo es 17

-- Regla de Sturges
SELECT @IntervaloSturges = 1 + (3.322 * LOG(COUNT(*)))
FROM HumanResources.v_Employee;

SELECT @IntervaloSturges; -- El intervalo es 19.8

-- Como la sugerencia es de 5 a 15 intervalos, vamos a usar solamente 6, esto
-- a criterio del profesor, para hacerlo mas simple.

-- Determinar el ancho del intervalo. El ancho es 6.
SELECT 40/6;

-- Calcular los límites inferiores
SELECT MIN(Age)
FROM HumanResources.v_Employee;
-- Los lims: 30, 36, 42, 48, 54, 60, 66

-- Calcular los límites superiores
SELECT MAX(Age)
FROM HumanResources.v_Employee;
-- Los lims: 35, 41, 47, 53, 59, 65, 71

-- Frecuencia de absoluta de Edad (datos agrupados)
SELECT AgeBucket
      ,COUNT(*) AS Frequency
FROM HumanResources.v_Employee
GROUP BY AgeBucket;

-- Frecuencias datos no agrupados (estrato para asalariados)
DECLARE @TotalRegistros INT = (SELECT COUNT(*) 
                               FROM HumanResources.v_Employee
                               WHERE SalariedFlag = N'Yes');
WITH EmployeeAgeCalc AS
(SELECT Age
       ,COUNT(*) AS Frecuencia
       ,CAST(100. * (COUNT(*)) /@TotalRegistros AS NUMERIC(5,3)) AS FrecuenciaRelativa
 FROM HumanResources.v_Employee
 WHERE SalariedFlag = N'Yes'
 GROUP BY Age
)
SELECT Age
      ,Frecuencia AS FrecuenciaAbsoluta
      ,SUM(Frecuencia) OVER (ORDER BY Age ASC
                             ROWS BETWEEN UNBOUNDED PRECEDING
                               AND CURRENT ROW
                            ) AS FrecuenciaAbsolutaAcumulada
      ,FrecuenciaRelativa
      ,SUM(FrecuenciaRelativa) OVER (ORDER BY Age ASC
                                     ROWS BETWEEN UNBOUNDED PRECEDING
                                      AND CURRENT ROW
                                    ) AS FrecuenciaRelativaAcumulada
FROM EmployeeAgeCalc
ORDER BY Age ASC;

-- Frecuencias datos agrupadas (estrato para asalariados)
DECLARE @TotalRegistros INT = (SELECT COUNT(*) 
                               FROM HumanResources.v_Employee
                               WHERE SalariedFlag = N'Yes');
WITH AgeBucketSQL AS
(SELECT AgeBucket
 FROM HumanResources.v_Employee
 WHERE SalariedFlag = N'Yes'
),
EmployeeAgeCalc AS
(SELECT AgeBucket
       ,COUNT(*) AS Frecuencia
       ,CAST(100. * (COUNT(*)) /@TotalRegistros AS NUMERIC(5,3)) AS FrecuenciaRelativa
 FROM AgeBucketSQL
 GROUP BY AgeBucket
)
SELECT AgeBucket
      ,Frecuencia AS FrecuenciaAbsoluta
      ,SUM(Frecuencia) OVER (ORDER BY AgeBucket ASC
                             ROWS BETWEEN UNBOUNDED PRECEDING
                               AND CURRENT ROW
                            ) AS FrecuenciaAbsolutaAcumulada
      ,FrecuenciaRelativa
      ,SUM(FrecuenciaRelativa) OVER (ORDER BY AgeBucket ASC
                                     ROWS BETWEEN UNBOUNDED PRECEDING
                                      AND CURRENT ROW
                                    ) AS FrecuenciaRelativaAcumulada
FROM EmployeeAgeCalc
ORDER BY AgeBucket ASC;

-- Datos para análsis bidimensional (tabla de contingencia)
SELECT AgeBucket
      ,Gender
FROM HumanResources.v_Employee
WHERE SalariedFlag = N'Yes'
ORDER BY AgeBucket;