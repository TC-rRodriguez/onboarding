using OpenClose.Interfaces;
using OpenClose.Resources;

namespace OpenClose.Services;

/// <summary>
/// Provides salary calculation for part-time employees.
/// </summary>
public class PartTimeSalaryCalculator : ISalaryCalculator
{
    /// <summary>
    /// Calculates the salary for a part-time employee based on hours worked.
    /// </summary>
    /// <param name="hoursWorked">The number of hours worked.</param>
    /// <returns>The calculated salary.</returns>
    public decimal CalculateSalary(int hoursWorked)
    {
        if (hoursWorked is < 0 or > 744)
        {
            throw new ArgumentOutOfRangeException(nameof(hoursWorked), "Hours worked must be between 0 and 744.");
        }
        
        var salary = hoursWorked * SalaryConstants.PartTimeEffortCompensation;
        
        if (hoursWorked > 160)
        {
            salary += GetExtraSalary(hoursWorked);
        }
        
        return salary;
    }
    
    /// <summary>
    /// Calculates the extra salary for hours worked beyond 160 hours.
    /// </summary>
    /// <param name="hoursWorked">The number of hours worked.</param>
    /// <returns>The extra salary for the additional hours worked.</returns>
    private static decimal GetExtraSalary(int hoursWorked)
    {
        var extraDays = hoursWorked - 160;
        return SalaryConstants.PartTimeEffortCompensation * extraDays;
    }
}