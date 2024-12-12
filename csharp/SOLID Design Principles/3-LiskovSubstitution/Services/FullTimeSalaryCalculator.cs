using Liskov.Interfaces;

namespace Liskov.Services;

public class FullTimeSalaryCalculator : ISalaryCalculator
{
    public decimal CalculateSalary(int hoursWorked, int extraHours)
    {
        const decimal hourValue = 50;
        return hourValue * (hoursWorked + extraHours);
    }
}