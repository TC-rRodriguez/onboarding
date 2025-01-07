using Liskov.Entities;

namespace Liskov.Services;

/// <summary>
/// Provides payroll services for calculating and printing employee salaries.
/// </summary>
public static class Payroll
{
    /// <summary>
    /// Calculates the monthly salary for a list of employees and prints the result to the console.
    /// </summary>
    /// <param name="employees">The list of employees.</param>
    public static void CalculateSalaryMonthly(List<BaseEmployee> employees)
    {
        foreach (var employee in employees)
        {
            Console.WriteLine($"Employee: {employee.FullName}, Payment: {employee.CalculateSalary():C1} ");
        }
    }
}