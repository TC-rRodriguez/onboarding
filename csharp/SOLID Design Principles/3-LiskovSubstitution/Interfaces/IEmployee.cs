namespace Liskov.Interfaces;

/// <summary>
/// Represents an employee.
/// </summary>
public interface IEmployee
{
    /// <summary>
    /// Gets the full name of the employee.
    /// </summary>
    string FullName { get; }

    /// <summary>
    /// Gets the number of hours worked by the employee.
    /// </summary>
    int HoursWorked { get; }
    
    /// <summary>
    /// Gets the number of extra hours worked by the employee.
    /// </summary>
    int ExtraHours { get; }
    
    /// <summary>
    /// Calculates the salary of the employee based on the hours worked and extra hours.
    /// </summary>
    /// <returns>The calculated salary as a decimal.</returns>
    decimal CalculateSalary();
}