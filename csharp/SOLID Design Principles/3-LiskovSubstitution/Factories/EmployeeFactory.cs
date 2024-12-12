using Liskov.Resources;
using Liskov.Services;
using Liskov.Entities;

namespace Liskov.Factories;

/// <summary>
/// Factory class for creating employee instances.
/// </summary>
public static class EmployeeFactory
{
    /// <summary>
    /// Creates an employee instance based on the provided type.
    /// </summary>
    /// <param name="fullName">The full name of the employee.</param>
    /// <param name="hoursWorked">The number of hours worked by the employee.</param>
    /// <param name="extraHours">The number of extra hours worked by the employee.</param>
    /// <param name="employeeType">The type of the employee (e.g., "FullTime", "Contractor").</param>
    /// <returns>An instance of <see cref="BaseEmployee"/>.</returns>
    /// <exception cref="ArgumentException">Thrown when the employee type is invalid.</exception>
    public static BaseEmployee CreateEmployee(string fullName, int hoursWorked, int extraHours, EmployeeType employeeType)
    {
        return employeeType switch
        {
            EmployeeType.FullTime => new EmployeeFullTime(fullName, hoursWorked, extraHours, new FullTimeSalaryCalculator()),
            EmployeeType.Contractor => new EmployeeContractor(fullName, hoursWorked, extraHours, new ContractorSalaryCalculator()),
            _ => throw new ArgumentException($"Invalid employee type: {employeeType}")
        };
    }
}