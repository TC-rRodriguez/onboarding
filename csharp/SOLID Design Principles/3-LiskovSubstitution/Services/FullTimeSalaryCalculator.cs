using Liskov.Interfaces;

namespace Liskov.Services;

/// <inheritdoc cref="ISalaryCalculator"/>
public class FullTimeSalaryCalculator : ISalaryCalculator
{
    public decimal CalculateSalary(int hoursWorked, int extraHours)
    {
        const decimal hourValue = 50;
        return hourValue * (hoursWorked + extraHours);
    }
}