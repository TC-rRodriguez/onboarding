using Liskov.Interfaces;

namespace Liskov.Services;

public class ContractorSalaryCalculator : ISalaryCalculator
{
    public decimal CalculateSalary(int hoursWorked, int extraHours)
    {
        const decimal hourValue = 40;
        return hourValue * (hoursWorked + extraHours);
    }
}