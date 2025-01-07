using OpenClose.Interfaces;
using OpenClose.Resources;

namespace OpenClose.Services;

/// <summary>
/// Provides salary calculation for full-time employees.
/// </summary>
public class FullTimeSalaryCalculator : ISalaryCalculator
{
    /// <summary>
    /// Calculates the salary for a full-time employee based on hours worked.
    /// </summary>
    /// <param name="hoursWorked">The number of hours worked.</param>
    /// <returns>The calculated salary.</returns>
    public decimal CalculateSalary(int hoursWorked)
    {
        if (hoursWorked is < 0 or > 744)
        {
            throw new ArgumentOutOfRangeException(nameof(hoursWorked), "Hours worked must be between 0 and 744.");
        }
        
        return hoursWorked * SalaryConstants.FullTimeHourValue;
    }
}
