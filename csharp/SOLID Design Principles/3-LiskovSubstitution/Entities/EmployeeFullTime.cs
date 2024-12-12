using Liskov.Interfaces;

namespace Liskov.Entities;

public class EmployeeFullTime(string fullname, int hoursWorked, int extraHours, ISalaryCalculator salaryCalculator) 
    : BaseEmployee(fullname, hoursWorked, extraHours, salaryCalculator);