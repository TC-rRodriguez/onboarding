using OpenClose.Entities;
using OpenClose.Interfaces;
using OpenClose.Resources;
using OpenClose.Services;

namespace OpenClose.Factories;

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
    /// <param name="employeeType">The type of the employee (e.g., "FullTime", "PartTime").</param>
    /// <returns>An instance of <see cref="IEmployee"/>.</returns>
    /// <exception cref="ArgumentException">Thrown when the employee type is invalid.</exception>
    public static IEmployee CreateEmployee(string fullName, int hoursWorked, EmployeeType employeeType)
    {
        return employeeType switch
        {
            EmployeeType.FullTime => new FullTimeEmployee(fullName, hoursWorked, new FullTimeSalaryCalculator()),
            EmployeeType.PartTime => new PartTimeEmployee(fullName, hoursWorked, new PartTimeSalaryCalculator()),
            _ => throw new ArgumentException($"Invalid employee type: {employeeType}")
        };
    }
}