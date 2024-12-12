namespace OpenClose.Interfaces;

/// <summary>
/// Represents an employee.
/// </summary>
public interface IEmployee
{
    /// <summary>
    /// Gets or sets the full name of the employee.
    /// </summary>
    string FullName { get; set; }

    /// <summary>
    /// Gets or sets the number of hours worked by the employee.
    /// </summary>
    int HoursWorked { get; set; }
    
    /// <summary>
    /// Calculates the salary of the employee based on the hours worked.
    /// </summary>
    /// <returns>The calculated salary.</returns>
    decimal CalculateSalary();
}