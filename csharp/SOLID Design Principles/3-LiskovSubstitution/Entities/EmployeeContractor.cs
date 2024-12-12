using Liskov.Interfaces;

namespace Liskov.Entities;

public class EmployeeContractor(string fullname, int hoursWorked, int extraHours, ISalaryCalculator salaryCalculator) 
    : BaseEmployee(fullname, hoursWorked, extraHours, salaryCalculator);