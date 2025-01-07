using Liskov.Factories;
using Liskov.Resources;
using Liskov.Services;

Payroll.CalculateSalaryMonthly([
    EmployeeFactory.CreateEmployee("Pepito Pérez", 160, 10, EmployeeType.FullTime),
    EmployeeFactory.CreateEmployee("Manuel Lopera", 180, 0, EmployeeType.Contractor)
]);

Console.WriteLine("Press any key to finish...");
Console.ReadKey();