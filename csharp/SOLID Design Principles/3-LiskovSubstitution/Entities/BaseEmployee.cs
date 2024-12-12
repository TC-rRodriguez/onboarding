using Liskov.Interfaces;

namespace Liskov.Entities;

/// <summary>
/// Represents a base employee.
/// </summary>
/// <param name="fullname">The full name of the employee.</param>
/// <param name="hoursWorked">The number of hours worked by the employee. Must be between 0 and 744.</param>
/// <param name="extraHours">The number of extra hours worked by the employee. Must be between 0 and 744.</param>
/// <param name="salaryCalculator">The salary calculator used to calculate the employee's salary.</param>
public abstract class BaseEmployee(string fullname, int hoursWorked, int extraHours, ISalaryCalculator salaryCalculator) : IEmployee
{
    public string FullName { get; } = fullname;
    public int HoursWorked { get; } = hoursWorked is < 0 or > 744
        ? throw new ArgumentOutOfRangeException(nameof(hoursWorked), "Hours worked must be between 0 and 744.")
        : hoursWorked;
    
    public int ExtraHours { get; } = extraHours is < 0 or > 744
        ? throw new ArgumentOutOfRangeException(nameof(extraHours), "Extra hours worked must be between 0 and 744.")
        : extraHours;

    public virtual decimal CalculateSalary()
    {
        return salaryCalculator.CalculateSalary(HoursWorked, extraHours);
    }
}