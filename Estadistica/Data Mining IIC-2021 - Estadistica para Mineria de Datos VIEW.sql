CREATE OR ALTER VIEW HumanResources.v_Employee AS
SELECT JobTitle
      ,DATEDIFF(YYYY,BirthDate,GETDATE()) AS Age
      ,CASE MaritalStatus
         WHEN N'M' THEN N'Married'
         WHEN N'S' THEN N'Single'
         ELSE N'N/A'
       END AS MaritalStatus
      ,CASE Gender
          WHEN N'M' THEN 'Male'
          WHEN N'F' THEN 'Female'
          ELSE N'N/A' 
       END AS Gender
      ,DATEDIFF(YYYY,HireDate,GETDATE()) AS YearsWorking
      ,CASE SalariedFlag
         WHEN 1 THEN N'Yes'
         WHEN 0 THEN N'No'
         ELSE N'N/A'
       END AS SalariedFlag
      ,VacationHours
      ,SickLeaveHours
      ,CASE CurrentFlag
         WHEN 1 THEN N'Yes'
         WHEN 0 THEN N'No'
         ELSE N'N/A'
       END AS CurrentFlag
      ,CASE 
          WHEN DATEDIFF(YYYY,BirthDate,GETDATE())>=30 AND
               DATEDIFF(YYYY,BirthDate,GETDATE())<=35 THEN N'30-35'
          WHEN DATEDIFF(YYYY,BirthDate,GETDATE())>=36 AND
               DATEDIFF(YYYY,BirthDate,GETDATE())<=41 THEN N'36-41'
          WHEN DATEDIFF(YYYY,BirthDate,GETDATE())>=42 AND
               DATEDIFF(YYYY,BirthDate,GETDATE())<=47 THEN N'42-47'
          WHEN DATEDIFF(YYYY,BirthDate,GETDATE())>=48 AND
               DATEDIFF(YYYY,BirthDate,GETDATE())<=53 THEN N'48-53'
          WHEN DATEDIFF(YYYY,BirthDate,GETDATE())>=54 AND
               DATEDIFF(YYYY,BirthDate,GETDATE())<=59 THEN N'54-59'
          WHEN DATEDIFF(YYYY,BirthDate,GETDATE())>=60 AND
               DATEDIFF(YYYY,BirthDate,GETDATE())<=65 THEN N'60-65'
          WHEN DATEDIFF(YYYY,BirthDate,GETDATE())>=66 AND
               DATEDIFF(YYYY,BirthDate,GETDATE())<=71 THEN N'66-71'
          WHEN DATEDIFF(YYYY,BirthDate,GETDATE())> 71 THEN N'71+'
          ELSE N'N/A'
        END AS AgeBucket
       ,CASE 
          WHEN DATEDIFF(YYYY,HireDate,GETDATE())>=8 AND
               DATEDIFF(YYYY,HireDate,GETDATE())<=10 THEN N'8-10'
          WHEN DATEDIFF(YYYY,HireDate,GETDATE())>=11 AND
               DATEDIFF(YYYY,HireDate,GETDATE())<=13 THEN N'11-13'
          WHEN DATEDIFF(YYYY,HireDate,GETDATE())>=14 THEN N'14-16'
          ELSE N'N/A'
        END AS YearsWorkingBucket
      ,CASE 
          WHEN VacationHours < 1 THEN N'0'
          WHEN VacationHours >=1 AND
               VacationHours <=20 THEN N'1-20'
          WHEN VacationHours >=21 AND
               VacationHours <=40 THEN N'21-40'
          WHEN VacationHours >=41 AND
               VacationHours <=60 THEN N'41-60'
          WHEN VacationHours >=61 AND
               VacationHours <=80 THEN N'61-80'  
          WHEN VacationHours >=81 AND
               VacationHours <=100 THEN N'81-100' 
          WHEN VacationHours >100 THEN N'100+'
          ELSE 'N/A'
        END AS VacationHoursBucket
       ,CASE 
          WHEN SickLeaveHours < 20 THEN N'20-'
          WHEN SickLeaveHours >=20 AND
               SickLeaveHours <=35 THEN N'20-35'
          WHEN SickLeaveHours >=36 AND
               SickLeaveHours <=55 THEN N'36-55'
          WHEN SickLeaveHours >=56 AND
               SickLeaveHours <=75 THEN N'56-75'
          WHEN SickLeaveHours >=76 AND
               SickLeaveHours <=85 THEN N'76-85'   
          WHEN SickLeaveHours >85 THEN N'85+'
          ELSE 'N/A'
        END AS SickLeaveHoursBucket
FROM HumanResources.Employee;