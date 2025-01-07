namespace OpenClose.Interfaces;

/// <summary>
/// Interface for calculating salary.
/// </summary>
public interface ISalaryCalculator
{
    /// <summary>
    /// Calculates the salary based on hours worked.
    /// </summary>
    /// <param name="hoursWorked">The number of hours worked.</param>
    /// <returns>The calculated salary.</returns>
    decimal CalculateSalary(int hoursWorked);
}