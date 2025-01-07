using OpenClose.Interfaces;

namespace OpenClose.Entities;

/// <summary>
/// Represents a part-time employee.
/// </summary>
/// <param name="fullname">The full name of the employee.</param>
/// <param name="hoursWorked">The number of hours worked by the employee.</param>
/// <param name="salaryCalculator">The salary calculator used to calculate the employee's salary.</param>
public class PartTimeEmployee(string fullname, int hoursWorked, ISalaryCalculator salaryCalculator) : IEmployee
{
    public string FullName { get; set; } = fullname;
    public int HoursWorked { get; set; } = hoursWorked;

    /// <summary>
    /// Calculates the salary of the employee based on the hours worked.
    /// </summary>
    /// <returns>The calculated salary.</returns>
    public decimal CalculateSalary()
    {
        return salaryCalculator.CalculateSalary(HoursWorked);
    }
}