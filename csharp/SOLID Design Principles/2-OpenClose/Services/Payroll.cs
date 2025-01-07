using OpenClose.Interfaces;

namespace OpenClose.Services;

/// <summary>
/// Provides payroll services for calculating employee salaries.
/// </summary>
public static class Payroll
{
    /// <summary>
    /// Calculates the monthly salary for a list of employees and prints the result.
    /// </summary>
    /// <param name="employees">The list of employees.</param>
    public static void CalculateSalaryMonthly(List<IEmployee> employees)
    {
        foreach (var employee in employees)
        {
            Console.WriteLine($"Employee: {employee.FullName}, Payment: {employee.CalculateSalary():C1} ");
        }
    }
}