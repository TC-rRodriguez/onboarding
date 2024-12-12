namespace Liskov.Interfaces;

/// <summary>
/// Interface for calculating salary.
/// </summary>
public interface ISalaryCalculator
{
    /// <summary>
    /// Calculates the salary based on hours worked and extra hours.
    /// </summary>
    /// <param name="hoursWorked">The number of hours worked.</param>
    /// <param name="extraHours">The number of extra hours worked.</param>
    /// <returns>The calculated salary.</returns>
    decimal CalculateSalary(int hoursWorked, int extraHours);
}