using Liskov.Interfaces;

namespace Liskov.Services;

/// <inheritdoc cref="ISalaryCalculator"/>
public class ContractorSalaryCalculator : ISalaryCalculator
{
    public decimal CalculateSalary(int hoursWorked, int extraHours)
    {
        const decimal hourValue = 40;
        return hourValue * (hoursWorked + extraHours);
    }
}